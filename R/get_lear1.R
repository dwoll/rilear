#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## get LEAR for 1 person for 1 cancer site
## based on total ERR / EAR over all exposure events
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

get_lear1 <- function(cancer_site, err_ear, d_base_cancer,
                      sex, age_attnd, surv_base, agex_1st, age_max, metric) {
    wt_transfer  <- cancer_site[["wt_transfer"]]
    idx_agex_1st <- which(age_attnd == agex_1st)
    cancer_base  <- d_base_cancer[[paste0("rate_", sex)]]
    
    ## divide by survival at earliest age at exposure for conditional survival
    ## - baseline survival because no difference up to exposure
    surv_base_agex_1st <- surv_base[idx_agex_1st]
    idx_keep <- (age_attnd > agex_1st) & (age_attnd <= age_max)
    
    ## functions based on ERR
    l_f_elr_err <- list(
        LEAR=function(err) {
            (cancer_base*err*surv_base)[idx_keep]    / surv_base_agex_1st
        },
        REID=function(err) {
            (cancer_base*err*surv_exposed)[idx_keep] / surv_base_agex_1st
        },
        ELR=function(err) {
            risk_0 <- (cancer_base        *surv_base)[idx_keep]
            risk_E <- (cancer_base*(1+err)*surv_exposed)[idx_keep]
            (risk_E - risk_0) / base_surv_agex_1st
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
            (ear*surv_base)[idx_keep]    / surv_base_agex_1st
        },
        REID=function(ear) {
            (ear*surv_exposed)[idx_keep] / surv_base_agex_1st
        },
        ELR=function(ear) {
            ## note: same EAR function es for REID
            (ear*surv_exposed)[idx_keep] / surv_base_agex_1st
        },
        RADS=function(ear) {
            ## CAVE
            ## not actually the excess risk, but just the part
            ## in the exp(-sum(.)), see below
            ear[idx_keep]
        })
    
    #####-----------------------------------------------------------------------
    ## excess risk at max age: sum attained age-specific excess risk over ages
    m_elr_err_ear <- vapply(metric, function(m) {
        elr_err <- sum(l_f_elr_err[[m]](err_ear[["err"]]))
        elr_ear <- sum(l_f_elr_ear[[m]](err_ear[["ear"]]))
        
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
