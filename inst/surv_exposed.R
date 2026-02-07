    #####-----------------------------------------------------------------------
    ## survival exposed from mortality rates exposed
    ## Surv_E = exp(-cumsum(q_E)) = Surv_0 * exp(-cumsum(r_mortality_0*ERR))
    ## q_E    = q_0 + (r_mortality_0 * ERR_mortality)
    if(any(metric %in% c("REID", "ELR"))) {
        ## matrix: rows = attained age, cols = age at exposure
        l_err_ear_mort_a0 <- lapply(seq_along(agex), function(i) {
            err <- f_err_mort(param_err_mort,
                              dose=dose[i],
                              agex=agex[i],
                              age =age_att,
                              sex =sex_n)

            ear <- f_ear_mort(param_ear_mort,
                              dose=dose[i],
                              agex=agex[i],
                              age =age_att,
                              sex =sex_n)
            
            lat <- f_latency(tse   =age_att - agex[i],
                             t0    =lat_t0,
                             eta   =lat_eta,
                             method=lat_method)
          
            list(err_mort=(1/ddref[i])*err*lat,
                 ear_mort=(1/ddref[i])*ear*lat)
        })

        ## total mortality ERR / EAR per attained age
        # = sum of exposure-specific ERRs / EARs
        l_err_ear_mort_a <- inv_l_basic(l_err_ear_mort_a0)
        err_mort <- rowSums(bind_cols(l_err_ear_mort_a[["err_mort"]]), na.rm=TRUE)
        ear_mort <- rowSums(bind_cols(l_err_ear_mort_a[["ear_mort"]]), na.rm=TRUE)
        
        ## excess force of cancer mortality from weighted ERR-EAR risk transfer
        rate_cancer_mort <- d_base_cancer_mort[[paste0("rate_", sex)]]
        q_excess <- wt_transfer["ERR"]*err_mort*rate_cancer_mort +
                    wt_transfer["EAR"]*ear_mort
        
        ## q_excess may be NA due to latency function -> set to 0
        q_excess[!is.finite(q_excess)] <- 0
        q_exposed <- q_base + q_excess
        
        ## survival exposed at age a -> hazard summed up to a-1
        ## Surv_E =        exp(-cumsum(q_exposed))
        ## Surv_E =        exp(-cumsum(q_base + q_excess))
        ## Surv_E = Surv_0*exp(-cumsum(q_excess))
        ## same: surv_base*exp(-cumsum(q_excess))
        surv_exposed <- lag(exp(-cumsum(q_exposed)), n=1L, default=1)
    }
