#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate the lifetime excess absolute risk due to radiation exposure
## metrics: LAR / LEAR / CER, REID / REIC, ELR, RADS
## background
## Ulanowski et al. Radiat Environ Biophys 2020. DOI 10.1007/s00411-020-00866-7
## Sommer et al. Radiat Res 2025. DOI: 10.1667/RADE-24-00060.1
## Sasaki et al. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

## for 1 cancer site
## sum ERR / EAR for each attained age over exposure events
sum_err_ear_a <- function(cancer_site, err_ear_a) {
    site_name <- cancer_site[["cancer_site"]]
    l_err_ear_sel0 <- lapply(err_ear_a, function(ee) {
        if(hasName(ee, site_name)) {
            ee_sel <- ee[[site_name]]
            list(err =ee_sel[["err"]],
                 ear =ee_sel[["ear"]],
                 agex=ee_sel[["agex"]])
        } else {
            NULL
        }
    })
    
    l_err_ear_sel <- Filter(Negate(is.null), l_err_ear_sel0)
    err  <- do.call(cbind, lapply(l_err_ear_sel, function(ees) { ees[["err"]] }))
    ear  <- do.call(cbind, lapply(l_err_ear_sel, function(ees) { ees[["ear"]] }))
    agex <- vapply(l_err_ear_sel, function(ees) { ees[["agex"]] }, FUN.VALUE=numeric(1))
    list(err =rowSums(err, na.rm=TRUE),
         ear =rowSums(ear, na.rm=TRUE),
         agex=agex)
}

## get LEAR for 1 person
## for all exposure events
## for all cancer sites
## for all attained ages
get_lear_indiv <- function(## parameters with uncertainty - in list l_param
    ## exposure:    for each exposure event:
    ##              sex, agex, dose (Gy or Sv), ddref, dose_rate,
    ##              cancer_site
    ## cancer_site: for each cancer site:
    ##              param_err / ear / err_mort / ear_mort,
    ##              wt_transfer, lat_t0, lat_eta
    l_param,
    ## parameters without uncertainty
    ## list with one component per cancer site
    ## TODO
    ## each component list with two components: outcome, mortality
    risk_model,
    risk_model_mort,
    ## list with one component per cancer site
    ## TODO
    ## each component list with two components: outcome, mortality
    base_cancer,
    base_cancer_mort,
    ## settings independent from exposure event, cancer_site
    ## overall mortality rates
    d_base_mort,
    age_max,
    lat_method=c("ProZES", "RadRAT"),
    metric=c("LAR", "LEAR", "CER", "REID", "REIC", "ELR", "RADS")) {
    lat_method <- match.arg(lat_method)
    age_attnd  <- d_base_mort[["age_n"]] # attained age
    metric     <- match.arg(metric, several.ok=TRUE)
    
    ## map metrics to unique names
    metric <- map_metric(metric)
    do_surv_exposed <- any(metric %in% c("REID", "ELR"))
    
    #####-----------------------------------------------------------------------
    ## for each exposure event: get ERR / EAR for each cancer site
    l_err_ear_a0 <- lapply(l_param[["exposure"]],
                           get_err_ear1_n,
                           cancer_site=l_param[["cancer_site"]],
                           age_attnd  =age_attnd,
                           risk_model =risk_model,
                           age_max    =age_max,
                           lat_method =lat_method)
    
    ## components may be NULL if agex > age_max
    l_err_ear_a <- Filter(Negate(is.null), l_err_ear_a0)
    
    #####-----------------------------------------------------------------------
    ## for each cancer site: total ERR / EAR per attained age
    l_err_ear <- Map(sum_err_ear_a,
                     l_param[["cancer_site"]],
                     err_ear_a=list(l_err_ear_a))
    
    #####-----------------------------------------------------------------------
    ## if necessary & possible (REID / REIC, ELR, RADS)
    ## for each exposure event: get mortality ERR / EAR for each cancer site
    if(do_surv_exposed) {
        l_err_ear_mort_a0 <- lapply(l_param[["exposure"]],
                                    get_err_ear1_n,
                                    cancer_site=l_param[["cancer_site"]],
                                    age_attnd  =age_attnd,
                                    risk_model =risk_model_mort,
                                    age_max    =age_max,
                                    lat_method =lat_method)
        
        ## components may be NULL if agex > age_max
        l_err_ear_mort_a <- Filter(Negate(is.null), l_err_ear_mort_a0)
        l_err_ear_mort   <- Map(sum_err_ear_a,
                                l_param[["cancer_site"]],
                                err_ear_a=list(l_err_ear_mort_a))
    }
    
    #####-----------------------------------------------------------------------
    ## overall mortality rates baseline
    ## force of mortality (hazard) from life table ("q")
    ## or from mortality rate ("rate")
    sex <- vapply(l_param[["exposure"]], function(e) { e[["sex"]] },
                  FUN.VALUE=character(1)) |>
        unique()
    
    q_base <- if(hasName(d_base_mort, paste0("q_", sex))) {
        d_base_mort[[paste0("q_", sex)]]
    } else if(hasName(d_base_mort, paste0("rate_", sex))) {
        d_base_mort[[paste0("rate_", sex)]]
    }
    
    ## overall baseline survival at attained age a
    ## -> hazard summed up to a-1
    base_surv <- lag(exp(-cumsum(q_base)), n=1L, default=1)
    
    #####-----------------------------------------------------------------------
    ## for each cancer site
    ## CER / LEAR / REID / REIC / RADS
    cancer_sites <- vapply(l_param[["cancer_site"]], function(cs) {
        cs[["cancer_site"]] }, FUN.VALUE=character(1)) |>
        unique()
    
    ## earliest age at exposure over cancer sites
    agex_1st <- lapply(l_err_ear, function(ee) { ee[["agex"]] }) |>
        unlist() |>
        min()
    
    l_lear <- Map(get_lear1,
                  l_param[["cancer_site"]],
                  err_ear      =l_err_ear,
                  d_base_cancer=base_cancer[cancer_sites],
                  sex          =sex,
                  age_attnd    =list(age_attnd),
                  surv_base    =list(base_surv),
                  agex_1st     =agex_1st,
                  age_max      =age_max,
                  metric       =list(metric))
    
    #####-----------------------------------------------------------------------
    ## total CER / LEAR / REID / REIC / RADS
    lear_total <- colSums(do.call(rbind, l_lear))
    c(l_lear, total=list(lear_total)) |>
        bind_rows(.id="site")
}
