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

map_metric <- function(x) {
  ## map metrics to unique names
  c(LAR ="LEAR", LEAR="LEAR", CER="LEAR",
    REID="REID", REIC="REID",
    ELR ="ELR",
    RADS="RADS")[toupper(x)] |> unname() |> unique()
}

get_lear1 <- function(## parameters with uncertainty - in list l_param
                      # exposure = list(dose, agex)    # dose in Gy or Sv
                      # param_err,
                      # param_ear,
                      # param_err_mort,
                      # param_ear_mort,
                      # wt_transfer=c(ERR=1, EAR=0),
                      # ddref,
                      # lat_t0,   # latency function: central value
                      # lat_eta,  # latency function ProZES: width transition period
                      l_param,
                      ## parameters without uncertainty
                      sex=c("f", "m"),
                      risk_model,
                      risk_model_mort,
                      d_base_cancer,
                      d_base_cancer_mort,
                      d_base_mort,
                      age_max,
                      lat_method=c("ProZES", "RadRAT"),
                      metric=c("LAR", "LEAR", "CER", "REID", "REIC", "ELR", "RADS")) {
    lat_method <- match.arg(lat_method)
    metric     <- match.arg(metric, several.ok=TRUE)
    sex        <- match.arg(sex)
    sex_n      <- c(m=1, f=2)[sex]  # for use in LSS risk function

    ## map metrics to unique names
    metric <- map_metric(metric)
    
    #####-----------------------------------------------------------------------
    ## extract parameters from list - may be NULL if missing
    dose           <- l_param[["exposure"]][["dose"]]     # in Gy or Sv
    agex           <- l_param[["exposure"]][["agex"]]
    param_err      <- l_param[["param_err"]]
    param_ear      <- l_param[["param_ear"]]
    param_err_mort <- l_param[["param_err_mort"]]
    param_ear_mort <- l_param[["param_ear_mort"]]
    wt_transfer    <- l_param[["wt_transfer"]]
    ddref          <- l_param[["ddref"]]
    lat_t0         <- l_param[["lat_t0"]]
    lat_eta        <- l_param[["lat_eta"]]
    
    f_err <- risk_model[["err"]][["f"]]
    f_ear <- risk_model[["ear"]][["f"]]
    if(!missing(risk_model_mort)) { f_err_mort <- risk_model_mort[["err"]][["f"]] }
    if(!missing(risk_model_mort)) { f_ear_mort <- risk_model_mort[["ear"]][["f"]] }
    
    #####-----------------------------------------------------------------------
    ## input validation
    if(length(agex) != length(dose)) {
        stop("lengths of agex and dose must be equal")
    }
  
    if((length(wt_transfer) != 2L) ||
       any(wt_transfer < 0) ||
       (sum(names(wt_transfer) == c("ERR", "EAR")) != 2L)) {
        stop("invalid weights (wt_transfer) for ERR - EAR transfer")
    }
  
    wt_transfer <- wt_transfer / sum(wt_transfer)  # normalize to sum to 1

    if(age_max <= max(agex)) {
        idx  <- agex >= age_max
        agex <- agex[!idx]
        dose <- dose[!idx]
        warning("some agex >= age_max, removing these agex")
        if(length(agex) == 0L) {
            return(setNames(rep(NA_real_, length(metric)), metric))
        }
    }
    
    if((min(agex) < min(d_base_cancer[["age_n"]])) |
       (max(agex) > max(d_base_cancer[["age_n"]]))) {
        stop("agex must be within d_base_cancer$age_n")
    }
  
    if((age_max < min(d_base_cancer[["age_n"]])) |
       (age_max > max(d_base_cancer[["age_n"]]))) {
        stop("age_max must be within d_base_cancer$age_n")
    }
  
    if(any(diff(d_base_mort[["age_n"]]) != 1)) {
        stop("d_base_mort$age_n must be equally spaced by 1")
    }
  
    if(any(diff(d_base_cancer[["age_n"]]) != 1)) {
        stop("d_base_cancer$age_n must be equally spaced by 1")
    }

    if(is.null(param_err)) { param_err <- risk_model[["err"]][["param"]] }
    if(is.null(param_ear)) { param_ear <- risk_model[["ear"]][["param"]] }
    if(is.null(param_err_mort) && !missing(risk_model_mort)) {
        param_err_mort <- risk_model_mort[["err"]][["param"]]
    }
    
    if(is.null(param_ear_mort) && !missing(risk_model_mort)) {
        param_ear_mort <- risk_model_mort[["ear"]][["param"]]
    }
    
    #####-----------------------------------------------------------------------
    ## for each agex / dose: calc ERR / EAR using the risk model functions
    age_att     <- d_base_cancer[["age_n"]]              # all attained ages
    cancer_base <- d_base_cancer[[paste0("rate_", sex)]]
    
    ## get ERR for each attained age separately for each age at exposure
    ## matrix: rows = attained age, cols = agex
    m_err_a <- vapply(seq_along(agex), function(i) {
        err <- f_err(param_err,
                     dose=dose[i],
                     agex=agex[i],
                     age =age_att,
                     sex =sex_n)
        
        lat <- f_latency(tse   =age_att - agex[i],
                         t0    =lat_t0,
                         eta   =lat_eta,
                         method=lat_method)
        
        (1/ddref[i])*err*lat
    }, FUN.VALUE=numeric(length(age_att)))
        
    ## get EAR for each attained age separately for each age at exposure
    ## matrix: rows = attained age, cols = agex
    m_ear_a <- vapply(seq_along(agex), function(i) {
        ear <- f_ear(param_ear,
                     dose=dose[i],
                     agex=agex[i],
                     age =age_att,
                     sex =sex_n)
      
        lat <- f_latency(tse   =age_att - agex[i],
                         t0    =lat_t0,
                         eta   =lat_eta,
                         method=lat_method)
        
        (1/ddref[i])*ear*lat
    }, FUN.VALUE=numeric(length(age_att)))
        
    ## total ERR / EAR per attained age = sum of agex specific ERRs / EARs
    ## na.rm=TRUE because for later agex (later columns),
    ## err before exposure is NA
    err_sum_a <- rowSums(m_err_a, na.rm=TRUE)
    ear_sum_a <- rowSums(m_ear_a, na.rm=TRUE)

    #####-----------------------------------------------------------------------
    ## survival baseline
    ## force of mortality (hazard) from life table ("q")
    ## or from mortality rate ("rate")
    q_base <- if(hasName(d_base_mort, paste0("q_", sex))) {
        d_base_mort[[paste0("q_", sex)]]
    } else if(hasName(d_base_mort, paste0("rate_", sex))) {
        d_base_mort[[paste0("rate_", sex)]]
    }

    ## baseline survival at age a -> hazard summed up to a-1
    surv_base <- lag(exp(-cumsum(q_base)), n=1L, default=1)
    
    ## survival exposed
    ## Surv_E = exp(-cumsum(q_E)) = Surv_0 * exp(-cumsum(r_mortality_0*ERR))
    ## q_E    = q_0 + r_mortality_0 * ERR_mortality
    if(any(metric %in% c("REID", "REIC", "ELR", "RADS"))) {
        ## matrix: rows = attained age, cols = age at exposure
        m_err_mort_a <- vapply(seq_along(agex), function(i) {
            err <- f_err_mort(param_err_mort,
                              dose=dose[i],
                              agex=agex[i],
                              age =age_att,
                              sex =sex_n)
          
            lat <- f_latency(tse   =age_att - agex[i],
                             t0    =lat_t0,
                             eta   =lat_eta,
                             method=lat_method)
          
            (1/ddref[i])*err*lat
        }, FUN.VALUE=numeric(length(age_att)))

        ## matrix: rows = attained age, cols = age at exposure
        m_ear_mort_a <- vapply(seq_along(agex), function(i) {
            ear <- f_ear_mort(param_ear_mort,
                              dose=dose[i],
                              agex=agex[i],
                              age =age_att,
                              sex =sex_n)
          
            lat <- f_latency(tse   =age_att - agex[i],
                             t0    =lat_t0,
                             eta   =lat_eta,
                             method=lat_method)
          
            (1/ddref[i])*ear*lat
        }, FUN.VALUE=numeric(length(age_att)))
        
        ## total mortality ERR / EAR per attained age
        # = sum of exposure-specific ERRs / EARs
        err_mort <- rowSums(m_err_mort_a)
        ear_mort <- rowSums(m_ear_mort_a)
        
        ## excess force of cancer mortality from weighted ERR-EAR risk transfer
        rate_cancer_mort <- d_base_cancer_mort[[paste0("rate_", sex)]]
        q_excess <- wt_transfer["ERR"]*err_mort*rate_cancer_mort +
                    wt_transfer["EAR"]*ear_mort
        
        ## q_excess may be NA due to latency function -> set to 0
        q_excess[!is.finite(q_excess)] <- 0
        
        ## Surv_E =        exp(-cumsum(q_exposed))
        ## Surv_E =        exp(-cumsum(q_base + q_excess))
        ## Surv_E = Surv_0*exp(-cumsum(q_excess))
        q_exposed <- q_base + q_excess
        
        ## survival exposed at age a -> hazard summed up to a-1
        surv_exposed <- lag(exp(-cumsum(q_exposed)), n=1L, default=1)
    }
    
    #####-----------------------------------------------------------------------
    ## CER / LEAR / REID / REIC / RADS for each attained age after exposure
    idx_keep <- (age_att > min(agex)) & (age_att <= age_max)
    
    ## divide by survival at earliest age at exposure for conditional survival
    idx_agex_1st <- which(age_att == min(agex))   # earliest age at exposure
    
    ## functions based on ERR
    l_f_elr_err <- list(
        LEAR=function(err) {
            (cancer_base*err*surv_base)[idx_keep]    / surv_base[idx_agex_1st]
        },
        REID=function(err) {
            (cancer_base*err*surv_exposed)[idx_keep] / surv_exposed[idx_agex_1st]
        },
        ELR=function(err) {
            risk_0 <- (cancer_base        *surv_base)[idx_keep]    / surv_base[idx_agex_1st]
            risk_E <- (cancer_base*(1+err)*surv_exposed)[idx_keep] / surv_exposed[idx_agex_1st]
            risk_E - risk_0
        },
        RADS=function(err) {
            ## CAVE
            ## not actually the excess risk, but just the part
            ## in the exp(-sum(.)), see below
            (cancer_base*err)[idx_keep]
        })

    ## functions based on EAR
    l_f_elr_ear <- list(
        LEAR=function(ear) {
            (ear*surv_base)[idx_keep]    / surv_base[idx_agex_1st]
        },
        REID=function(ear) {
            (ear*surv_exposed)[idx_keep] / surv_exposed[idx_agex_1st]
        },
        ELR=function(ear) {
            ## note: same EAR function es for REID
            (ear*surv_exposed)[idx_keep] / surv_exposed[idx_agex_1st]
        },
        RADS=function(ear) {
            ## CAVE
            ## not actually the excess risk, but just the part
            ## in the exp(-sum(.)), see below
            ear_sum_a[idx_keep]
        })

    #####-----------------------------------------------------------------------
    ## excess risk at max age: sum attained age-specific excess risk over ages
    m_elr_err_ear <- vapply(metric, function(m) {
        elr_err <- sum(l_f_elr_err[[m]](err_sum_a))
        elr_ear <- sum(l_f_elr_ear[[m]](ear_sum_a))
        
        if(m == "RADS") {
            c(elr_err=1-exp(-elr_err),
              elr_ear=1-exp(-elr_ear))
        } else {
            c(elr_err=elr_err,
              elr_ear=elr_ear)
        }
    }, FUN.VALUE=numeric(2))

    ## final excess risk based on selected ERR / EAR transfer
    apply(m_elr_err_ear, MARGIN=2, FUN=weighted.mean, w=wt_transfer)
}
