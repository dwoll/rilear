#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## generate list of sets of parameters with uncertainty
## necessary for estimating lifetime excess absolute risk
## simulate parameters independently
## for each exposure event: dose, ddref
## for each cancer site: risk model coefficients, ERR-EAR transfer, latency
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

gen_param_mc <- function(exposure,
                         n_sim            =1000L,
                         wt_transfer,
                         lat_t0,
                         lat_eta,
                         lat_method       =c("ProZES", "RadRAT"),
                         lat_fixed        =FALSE,
                         wt_transfer_fixed=FALSE,
                         ddref_fixed      =FALSE,
                         ##
                         risk_model,
                         risk_model_mort) {
    rmm <- if(!missing(risk_model_mort)) {
        risk_model_mort
    } else {
        NULL
    }
    
    #####-----------------------------------------------------------------------
    ## for each exposure event: dose, ddref
    ## add sex, agex / timing, dose_rate, cancer site list
    #####-----------------------------------------------------------------------
    
    l_expo_mc <- lapply(exposure, sim_exposure, n_sim=n_sim, transpose=FALSE)
    # l_expo_mc  <- inv_l_dose(l_expo_mc0) # 1 component per n_sim

    #####-----------------------------------------------------------------------
    ## for each cancer site: risk model coefficients, ERR-EAR transfer, latency
    #####-----------------------------------------------------------------------
    
    ## get all cancer sites
    cancer_sites <- lapply(exposure, function(e) { e[["cancer_site"]] }) |>
        unlist() |>
        unique()

    l_cancer_site_mc <- lapply(cancer_sites, function(cs) {
        ## cancer site name
        cs_mc <- rep(cs, n_sim)
        
        #####-----------------------------------------------------------------------
        ## risk model parameters
        beta_err_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                        mean =risk_model[[cs]][["err"]][["param"]],
                                        sigma=risk_model[[cs]][["err"]][["varm"]])
        
        beta_ear_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                        mean =risk_model[[cs]][["ear"]][["param"]],
                                        sigma=risk_model[[cs]][["ear"]][["varm"]])
        
        ## cancer mortality model for REID / REIC, ELR, RADS
        if(!is.null(rmm)) {
            beta_err_mort_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                                 mean =rmm[[cs]][["err"]][["param"]],
                                                 sigma=rmm[[cs]][["err"]][["varm"]])
            
            beta_ear_mort_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                                 mean =rmm[[cs]][["ear"]][["param"]],
                                                 sigma=rmm[[cs]][["ear"]][["varm"]])
        } else {
            beta_err_mort_mc <- NULL
            beta_ear_mort_mc <- NULL
        }
        
        #####-----------------------------------------------------------------------
        ## weights for ERR-EAR transfer of risk
        ## TODO
        ## choose according to cancer site
        ## ProZES: uniform in [0, 1], otherwise suggest 0.5 ERR - 0.5 EAR
        ## RadRAT:
        ## general: 0.7 ERR - 0.3 EAR
        ## breast: 0 ERR - 1 EAR
        ## lung: 0.3 ERR - 0.7 EAR
        ## brain: 1 ERR - 0 EAR
        ## normalize to sum 1
        wt_transfer_cs  <- wt_transfer[[cs]] / sum(wt_transfer[[cs]])
        wt_transfer_err <- if(wt_transfer_fixed) {
            rep(wt_transfer_cs[["ERR"]], n_sim)
        } else {
            norm_var <- 0.05^2
            ## force norm_mean within [0.01, 0.99]
            norm_mean  <- 0.01 + 0.98*wt_transfer_cs[["ERR"]]
            beta_alpha <-    norm_mean  * ((norm_mean*(1-norm_mean) / norm_var)-1)
            beta_beta  <- (1-norm_mean) * ((norm_mean*(1-norm_mean) / norm_var)-1)
            rbeta(n_sim, shape1=beta_alpha, shape2=beta_beta)
        }
        
        stopifnot(all(wt_transfer_err >= 0),
                  all(wt_transfer_err <= 1))
        
        wt_transfer_mc <- cbind(ERR=  wt_transfer_err,
                                EAR=1-wt_transfer_err)
        
        #####-----------------------------------------------------------------------
        ## latency function
        ## TODO RadRAT
        ## the mid-point, t0, is described by the following triangular probability
        ## distributions:
        ## solid cancers other than thyroid, T.5; 7:5; 10;
        ## thyroid, T.3; 5; 7;
        ## leukaemia, T.2; 2:25; 2:5
        ## ProZES
        # val_eta <- c(solid=6.25, leuk_lymph=7.66)[cancer]
        lat_eta_mc <- rep(lat_eta[[cs]], n_sim) # no uncertainty in eta
        lat_t0_mc  <- if(lat_fixed) {
            rep(lat_t0[[cs]],  n_sim)
            # if(missing(lat_eta)) {
            #     rep(unname(val_eta), n_sim)
            # } else {
            #     rep(lat_eta, n_sim)
            # }
            
            # if(missing(lat_t0)) {
            #     val_t0 <- c(ProZES_solid     =4,
            #                 ProZES_leuk_lymph=1.5,
            #                 RadRAT_solid     =7.5,
            #                 RadRAT_leuk_lymph=2.25)[paste(lat_method, cancer, sep="_")]
            #     
            #     rep(unname(val_t0), n_sim)
            # } else {
            #     rep(lat_t0, n_sim)
            # }
        } else {
            runif(n_sim, min=0.8*lat_t0[[cs]], max=1.2*lat_t0[[cs]])
            # lat_eta_mc <- rep(unname(val_eta), n_sim)
            # val_t0     <- if(lat_method == "ProZES") {
            #     ## 1.25-1.75 leukemia, 3-4 years solid
            #     list(solid     =c(min=3,    max=5),
            #          leuk_lymph=c(min=1.25, max=1.75))[[cancer]]
            # } else if(lat_method == "RadRAT") {
            #     ## 2.25 years leukemia, 5 thyroid, 7.5 solid
            #     list(solid     =c(min=5, max=10),
            #          leuk_lymph=c(min=1, max=3.5))[[cancer]]
            # }
            # 
            # lat_t0_mc <- runif(n_sim, min=val_t0["min"], max=val_t0["max"])
        }
        
        l_cs0 <- list(cancer_site   =cs_mc,
                      param_err     =beta_err_mc,
                      param_ear     =beta_ear_mc,
                      param_err_mort=beta_err_mort_mc,
                      param_ear_mort=beta_ear_mort_mc,
                      wt_transfer   =wt_transfer_mc,
                      lat_t0        =lat_t0_mc,
                      lat_eta       =lat_eta_mc)
        
        Filter(Negate(is.null), l_cs0)
    })
    
    #####-----------------------------------------------------------------------
    ## join simulated parameters into list
    ## hierarchy (out -> in):
    ## MC run -> exposure no. -> dose, ... | site -> coef, wt_transfer, ...
    l_param <- lapply(seq_len(n_sim), function(k) {
        names_expo <- unlist(unique(lapply(l_expo_mc, names)))
        names_cs   <- unlist(unique(lapply(l_cancer_site_mc, names)))
        l_exposure <- lapply(seq_along(exposure), function(i) {
            l <- lapply(names_expo, function(j) {
                if(inherits(l_expo_mc[[i]][[j]], "matrix") ||
                   inherits(l_expo_mc[[i]][[j]], "data.frame")) {
                    l_expo_mc[[i]][[j]][k, , drop=TRUE]
                } else {
                    l_expo_mc[[i]][[j]][[k]]
                }
            })
            
            setNames(l, names_expo)
        })
        
        l_cancer_site <- lapply(seq_along(cancer_sites), function(i) {
            l <- lapply(names_cs, function(j) {
                if(inherits(l_cancer_site_mc[[i]][[j]], "matrix") ||
                   inherits(l_cancer_site_mc[[i]][[j]], "data.frame")) {
                    l_cancer_site_mc[[i]][[j]][k, , drop=TRUE]
                } else {
                    l_cancer_site_mc[[i]][[j]][[k]]
                }
            })
            
            setNames(l, names_cs)
        })
        
        list(exposure   =l_exposure,
             cancer_site=setNames(l_cancer_site, cancer_sites))
    })
    
    l_param
}
