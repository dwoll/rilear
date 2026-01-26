#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate ERR / EAR for given exposure
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## get ERR / EAR for 1 person
## for 1 exposure event
## for 1 cancer site
## for all attained ages
#####---------------------------------------------------------------------------

get_err_ear1_1 <- function(cancer_site,
                           risk_model,
                           age_attnd,
                           exposure,
                           lat_method) {
    sex   <- exposure[["sex"]]
    sex_n <- unname(c(m=1, f=2)[sex])  # for use in LSS risk function
    
    #####-----------------------------------------------------------------------
    ## extract parameters from exposure
    dose  <- exposure[["dose"]]     # in Gy or Sv
    agex  <- exposure[["agex"]]
    ddref <- if(hasName(exposure, "ddref")) {
        exposure[["ddref"]]
    } else {
        1
    }
    
    #####-----------------------------------------------------------------------
    ## extract parameters from cancer_site
    param_err   <- cancer_site[["param_err"]]
    param_ear   <- cancer_site[["param_ear"]]
    wt_transfer <- cancer_site[["wt_transfer"]]
    lat_t0      <- cancer_site[["lat_t0"]]
    lat_eta     <- cancer_site[["lat_eta"]]
    
    f_err <- risk_model[["err"]][["f"]]
    f_ear <- risk_model[["ear"]][["f"]]
    
    if((length(wt_transfer) != 2L) ||
       any(wt_transfer < 0)        ||
       (sum(names(wt_transfer) == c("ERR", "EAR")) != 2L)) {
        stop("invalid weights (wt_transfer) for ERR - EAR transfer")
    }
    
    ## normalize to sum to 1
    wt_transfer <- wt_transfer / sum(wt_transfer)
    
    #####-----------------------------------------------------------------------
    ## calc ERR / EAR using the risk model functions
    ## for all attained ages
    err <- f_err(param_err,
                 dose=dose,
                 agex=agex,
                 age =age_attnd,
                 sex =sex_n)
    
    ear <- f_ear(param_ear,
                 dose=dose,
                 agex=agex,
                 age =age_attnd,
                 sex =sex_n)
    
    lat <- f_latency(tse   =age_attnd - agex,
                     t0    =lat_t0,
                     eta   =lat_eta,
                     method=lat_method)
    
    list(err =(1/ddref)*err*lat,
         ear =(1/ddref)*ear*lat,
         agex=agex)
}

#####---------------------------------------------------------------------------
## get ERR / EAR for 1 person
## for 1 exposure event
## for all cancer sites
## for all attained ages
#####---------------------------------------------------------------------------

get_err_ear1_n <- function(exposure, cancer_site, age_attnd,
                           risk_model, age_max, lat_method) {
    if(!hasName(exposure, "agex")) {
        exposure[["agex"]] <- exposure[["agex_timing"]]
    }
    
    cancer_site_sel <- cancer_site[exposure[["cancer_site"]]]
    risk_model_sel  <- risk_model[ exposure[["cancer_site"]]]
    
    if(exposure[["agex"]] > age_max) {
        NULL
    } else {
        l_err_ear <- Map(get_err_ear1_1,
                         cancer_site_sel,      # list with 1 component per cancer site
                         risk_model=risk_model_sel,
                         age_attnd =list(age_attnd),
                         exposure  =list(exposure),
                         lat_method=lat_method)
        
        l_err_ear
    }
}

#####---------------------------------------------------------------------------
## for 1 cancer site: get excess force of cancer mortality (hazard)
## for surv_exposed (necessary for REID, ELR)
#####---------------------------------------------------------------------------

get_qE1 <- function(cancer_site, err_ear, d_base_cancer_mort, sex) {
    err_mort         <- err_ear[["err"]]
    ear_mort         <- err_ear[["ear"]]
    wt_transfer      <- cancer_site[["wt_transfer"]]
    cancer_base_mort <- d_base_cancer_mort[[paste0("rate_", sex)]]
    
    q_excess <- wt_transfer["ERR"]*err_mort*cancer_base_mort +
        wt_transfer["EAR"]*ear_mort
    
    q_excess
}
