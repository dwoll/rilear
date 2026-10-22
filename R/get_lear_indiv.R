#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate the lifetime excess absolute risk due to radiation exposure
## for an individual
## metrics: LAR / LEAR / CER, REID / REIC, ELR, RADS
## Ulanowski et al. Radiat Environ Biophys 2020. DOI 10.1007/s00411-020-00866-7
## Sommer et al. Radiat Res 2025. DOI: 10.1667/RADE-24-00060.1
## Sasaki et al. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## for 1 cancer site
## for all exposure events
## sum ERR / EAR for each attained age over all exposure events
#####---------------------------------------------------------------------------

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

    if(length(l_err_ear_sel) >= 1L) {
        err  <- do.call(cbind, lapply(l_err_ear_sel, function(ees) { ees[["err"]] }))
        ear  <- do.call(cbind, lapply(l_err_ear_sel, function(ees) { ees[["ear"]] }))
        agex <- vapply(l_err_ear_sel, function(ees) { ees[["agex"]] }, FUN.VALUE=numeric(1))
        list(err =rowSums(err, na.rm=TRUE),
             ear =rowSums(ear, na.rm=TRUE),
             agex=agex)
    } else {
        NULL
    }
}

#####---------------------------------------------------------------------------
## get LEAR for 1 person
## for all exposure events
## for all cancer sites
#####---------------------------------------------------------------------------

get_lear_indiv <- function(## parameters with uncertainty - in list l_param
                           ## exposure:    for each exposure event:
                           ##              sex, agex, dose (Gy or Sv), ddref, dose_rate,
                           ##              cancer_site
                           ## cancer_site: for each cancer site:
                           ##              cancer_site (name)
                           ##              param_err / ear / err_mort / ear_mort,
                           ##              wt_transfer, lat_t0, lat_eta
                           l_param,
                           ## parameters without uncertainty
                           ## list with one component per cancer site
                           risk_model,
                           risk_model_mort,
                           ## list with one component per cancer site
                           base_cancer,
                           base_cancer_mort,
                           ## settings independent from exposure event, cancer_site
                           ## overall population mortality rates
                           d_base_mort,
                           age_max,    # integrate risk up to (including) this age
                           lat_method=c("ProZES", "RadRAT"),
                           metric=c("LAR", "LEAR", "CER", "REID", "REIC", "ELR", "RADS")) {
    lat_method <- match.arg(lat_method)
    age_attnd  <- d_base_mort[["age_n"]] # attained age
    metric     <- match.arg(metric, several.ok=TRUE)

    ## map metrics to unique names
    metric <- map_metric(metric)

    ## necessary to separately estimate survival of exposed?
    do_surv_exposed <- any(metric %in% c("REID", "ELR"))
    if(do_surv_exposed) {
        if(missing(risk_model_mort) ||
           missing(base_cancer_mort)) {
            stop("Need cancer mortality risk model and baseline cancer rates.")
        }
    }

    ## extract sex and ensure length 1
    sex <- vapply(l_param[["exposure"]], function(e) { e[["sex"]] },
                  FUN.VALUE=character(1L)) |>
        unique()

    stopifnot(length(sex) == 1L)

    #####-----------------------------------------------------------------------
    ## for each exposure event: get ERR / EAR for each cancer site
    ## for outcome of interest
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
    ## sum over all exposure events
    l_err_ear0 <- Map(sum_err_ear_a,
                      l_param[["cancer_site"]],
                      err_ear_a=list(l_err_ear_a))

    l_err_ear <- Filter(Negate(is.null), l_err_ear0)

    #####-----------------------------------------------------------------------
    ## overall mortality rates baseline
    ## force of mortality (hazard) from life table ("q")
    ## or from mortality rate ("rate")
    q_base <- if(hasName(d_base_mort, paste0("q_", sex))) {
        d_base_mort[[paste0("q_", sex)]]
    } else if(hasName(d_base_mort, paste0("rate_", sex))) {
        d_base_mort[[paste0("rate_", sex)]]
    } else {
      stop("Need baseline overall mortality rate variable `q_f/m` or `rate_f/m`.")
    }

    ## overall baseline survival at attained age a
    ## -> hazard summed up to a-1
    surv_base <- dplyr::lag(exp(-cumsum(q_base)), n=1L, default=1)

    #####-----------------------------------------------------------------------
    ## if necessary & possible (REID / REIC, ELR)
    ## for each exposure event: get mortality ERR / EAR for each cancer site
    surv_exposed <- if(do_surv_exposed) {
        ## for each exposure event: get ERR / EAR for each cancer site
        l_err_ear_mort_a0 <- lapply(l_param[["exposure"]],
                                    get_err_ear1_n,
                                    cancer_site=l_param[["cancer_site"]],
                                    age_attnd  =age_attnd,
                                    risk_model =risk_model_mort,
                                    age_max    =age_max,
                                    lat_method =lat_method)

        ## components may be NULL if agex > age_max
        l_err_ear_mort_a <- Filter(Negate(is.null), l_err_ear_mort_a0)

        ## for each cancer site: total ERR / EAR per attained age
        ## sum over all exposure events
        l_err_ear_mort0  <- Map(sum_err_ear_a,
                                l_param[["cancer_site"]],
                                err_ear_a=list(l_err_ear_mort_a))

        l_err_ear_mort <- Filter(Negate(is.null), l_err_ear_mort0)

        ## for each cancer site: excess force of mortality
        ## extract cancer sites with ERR / EAR for mortality available
        ## CAVE
        ## l_err_ear_mort has NULL filtered out,
        ## make sure l_param[["cancer_site"]] and
        ## base_cancer_mort match order w/r/t cancer site
        cs_mort_have <- Reduce(intersect,
                               list(names(l_param[["cancer_site"]]),
                                    names(l_err_ear_mort),
                                    names(base_cancer_mort)))

        l_q_excess0 <- Map(get_qE1,
                           l_param[["cancer_site"]][cs_mort_have],
                           err_ear           =l_err_ear_mort[cs_mort_have],
                           d_base_cancer_mort=base_cancer_mort[cs_mort_have],
                           sex               =sex)

        l_q_excess <- Filter(Negate(is.null), l_q_excess0)

        ## total excess force of mortality
        q_excess <- rowSums(do.call(cbind, l_q_excess))
        q_excess[!is.finite(q_excess)] <- 0
        q_exposed <- q_base + q_excess

        ## survival exposed at age a -> hazard summed up to a-1
        ## Surv_E =        exp(-cumsum(q_exposed))
        ## Surv_E =        exp(-cumsum(q_base + q_excess))
        ## Surv_E = Surv_0*exp(-cumsum(q_excess))
        ## same: surv_base*exp(-cumsum(q_excess))
        dplyr::lag(exp(-cumsum(q_exposed)), n=1L, default=1)
    } else {
        NULL
    }

    #####-----------------------------------------------------------------------
    ## for each cancer site
    ## get CER / LEAR / REID / REIC / RADS
    if(length(l_err_ear) >= 1L) {
        ## earliest age at exposure over all cancer sites
        agex_1st <- lapply(l_err_ear, function(ee) { ee[["agex"]] }) |>
            unlist() |>
            min()

        ## CAVE
        ## l_err_ear has NULL filtered out,
        ## make sure l_param[["cancer_site"]] and
        ## base_cancer match order w/r/t cancer site
        cs_incid_have <- Reduce(intersect,
                               list(names(l_param[["cancer_site"]]),
                                    names(l_err_ear),
                                    names(base_cancer)))

        l_lear <- Map(get_lear1,
                      l_param[["cancer_site"]][cs_incid_have],
                      err_ear      =l_err_ear[cs_incid_have],
                      d_base_cancer=base_cancer[cs_incid_have],
                      sex          =sex,
                      age_attnd    =list(age_attnd),
                      surv_base    =list(surv_base),
                      surv_exposed =list(surv_exposed),
                      agex_1st     =agex_1st,
                      age_max      =age_max,
                      metric       =list(metric))

        #####-------------------------------------------------------------------
        ## total CER / LEAR / REID / REIC / RADS
        lear_total <- colSums(do.call(rbind, l_lear))
        c(l_lear, total=list(lear_total)) |>
            bind_rows(.id="site")
    } else {
        NULL
    }
}
