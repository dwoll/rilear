#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## generate list of sets of parameters with uncertainty
## necessary for estimating lifetime excess absolute risk
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

gen_param_mc <- function(n_sim            =1000L,
                         exposure,
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
    #####-----------------------------------------------------------------------
    ## simulate parameters independently
    #####-----------------------------------------------------------------------
    
    #####-----------------------------------------------------------------------
    ## risk model parameters
    beta_err_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                    mean =risk_model$err$param,
                                    sigma=risk_model$err$varm)
    
    beta_ear_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                    mean =risk_model$ear$param,
                                    sigma=risk_model$ear$varm)
    
    colnames(beta_err_mc) <- names(risk_model$err$param)
    colnames(beta_ear_mc) <- names(risk_model$ear$param)
    
    ## cancer mortality model for REID / REIC, ELR, RADS
    if(!missing(risk_model_mort) && !is.null(risk_model_mort)) {
        rmm <- risk_model_mort
        beta_err_mort_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                             mean =rmm$err$param,
                                             sigma=rmm$err$varm)
        
        beta_ear_mort_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                             mean =rmm$ear$param,
                                             sigma=rmm$ear$varm)
        
        colnames(beta_err_mort_mc) <- names(rmm$err$param)
        colnames(beta_ear_mort_mc) <- names(rmm$ear$param)
    } else {
        beta_err_mort_mc <- NULL
        beta_ear_mort_mc <- NULL
    }
    
    #####-----------------------------------------------------------------------
    ## dose - bind to matrix
    l_expo_mc    <- lapply(exposure, sim_dose, n_sim=n_sim)
    dose_mc      <- do.call(cbind,  lapply(l_expo_mc, function(e) { e[["dose"]] }))
    ddref_mc     <- do.call(cbind,  lapply(l_expo_mc, function(e) { e[["ddref"]] }))
    dose_rate_mc <- do.call(cbind,  lapply(l_expo_mc, function(e) { e[["dose_rate"]] }))
    
    #####-----------------------------------------------------------------------
    ## weights for ERR-EAR transfer of risk
    ## ProZES: uniform in [0, 1], otherwise suggest 0.5 ERR - 0.5 EAR
    ## RadRAT:
    ## general: 0.7 ERR - 0.3 EAR
    ## breast: 0 ERR - 1 EAR
    ## lung: 0.3 ERR - 0.7 EAR
    ## brain: 1 ERR - 0 EAR
    ## normalize to sum 1
    wt_transfer <- wt_transfer / sum(wt_transfer)
    wt_transfer_err <- if(wt_transfer_fixed) {
        rep(wt_transfer[["ERR"]], n_sim)
    } else {
        norm_var   <- 0.05^2
        norm_mean  <- wt_transfer[["ERR"]]
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
    if(lat_fixed) {
        lat_t0_mc  <- rep(lat_t0,  n_sim)
        lat_eta_mc <- rep(lat_eta, n_sim)
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
        lat_eta_mc <- rep(lat_eta, n_sim) # no uncertainty in eta
        lat_t0_mc  <- runif(n_sim, min=0.8*lat_t0, max=1.2*lat_t0)
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
    
    #####-----------------------------------------------------------------------
    ## join simulated parameters into list
    l_param0 <- list(exposure      =dose_mc,
                     ddref         =ddref_mc,
                     dose_rate     =dose_rate_mc,
                     param_err     =beta_err_mc,
                     param_ear     =beta_ear_mc,
                     param_err_mort=beta_err_mort_mc,
                     param_ear_mort=beta_ear_mort_mc,
                     wt_transfer   =wt_transfer_mc,
                     lat_t0        =lat_t0_mc,
                     lat_eta       =lat_eta_mc)
    
    ## remove NULL values and
    ## turn list inside-out such that each component
    ## is one parameter set for excess lifetime risk calculation
    l_param1 <- Filter(Negate(is.null), l_param0)
    l_param  <- c(inv_l_param(l_param1))
    l_param
}
