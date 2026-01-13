#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate the lifetime excess absolute risk due to radiation exposure
## simulate parameters that have uncertainty
## generate MC distribution of lifetime excess risk from sets of parameters
## then either aggregate to mean, median, CI, or return all MC results
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## helper function to pass one batch of parameter settings to get_elr1()
#####---------------------------------------------------------------------------

get_elr1_batch <- function(x, lat_method, ...) {
    l_out <- lapply(x, get_elr1, lat_method=lat_method, ...)
    do.call(rbind, l_out)
}

#####---------------------------------------------------------------------------
## function for MC simulation and collection of excess lifetime risk values
#####---------------------------------------------------------------------------

get_elr1_mc <- function(## parameters with uncertainty
                        exposure,
                        wt_transfer,
                        wt_transfer_fixed=FALSE,
                        ddref=1,
                        ddref_fixed=FALSE,
                        lat_t0,   # latency function: central value
                        lat_eta,  # latency function ProZES: width of transition period
                        lat_method=c("ProZES", "RadRAT"),
                        lat_fixed=FALSE,
                        ## parameters specific to MC
                        alpha       =0.05,
                        # cancer      =c("solid", "leuk_lymph"),
                        n_sim       =1000L,
                        multicore   =FALSE,
                        n_cores_max =10L,
                        n_cores_omit=2L,
                        aggr_mc     =TRUE,
                        ## parameters without uncertainty
                        ## passed through to get_elr1()
                        ...) {
    lat_method <- match.arg(lat_method)
    dots       <- list(...)

    #####-----------------------------------------------------------------------
    ## simulate parameters independently
    #####-----------------------------------------------------------------------

    #####-----------------------------------------------------------------------
    ## risk model parameters
    risk_model  <- dots[["risk_model"]]
    beta_err_mc <- mvtnorm::rmvnorm(n    =n_sim,
                                    mean =risk_model$err$param,
                                    sigma=risk_model$err$varm)

    beta_ear_mc <- mvtnorm::rmvnorm(n=n_sim,
                                    mean =risk_model$ear$param,
                                    sigma=risk_model$ear$varm)

    colnames(beta_err_mc) <- names(risk_model$err$param)
    colnames(beta_ear_mc) <- names(risk_model$ear$param)
    
    ## cancer mortality model for REID / REIC, ELR, RADS
    if(hasName(dots, "risk_model_mort")) {
        rmm <- dots[["risk_model_mort"]]
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
    expo_mc <- do.call(cbind,
                       lapply(exposure, sim_dose, n_sim=n_sim))
    
    #####-----------------------------------------------------------------------
    ## weights for ERR-EAR transfer of risk
    ## ProZES: uniform in [0, 1], otherwise suggest 0.5 ERR - 0.5 EAR
    ## RadRAT:
    ## general: 0.7 ERR - 0.3 EAR
    ## breast: 0 ERR - 1 EAR
    ## lung: 0.3 ERR - 0.7 EAR
    ## brain: 1 ERR - 0 EAR
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
    ## DDREF
    ddref_mc <- if(ddref_fixed) {
        rep(ddref, n_sim)
    } else {
        ## TODO ProZES
        ## TODO RadRAT
        ## threshold low dose rate UNSCEAR 2003: < 6 mGy/h
        ## here: choose DDREF in interval [0.8, 3] (ICRP)
        ## assume mean 1.5
        ## beta distribution in [0, 1] -> 1.5 corresponds to
        ## [0.8, 3]-0.8 / 2.2
        ## TODO
        ## make sure no DDREF for leukemia
        norm_var   <- (0.1 / 0.8)^2
        norm_mean  <- (1.5-0.8) / 2.2
        beta_alpha <-    norm_mean  * ((norm_mean*(1-norm_mean) / norm_var)-1)
        beta_beta  <- (1-norm_mean) * ((norm_mean*(1-norm_mean) / norm_var)-1)
      
        rbeta(n_sim, shape1=beta_alpha, shape2=beta_beta)*2.2 + 0.8
    }
    
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
    l_param0 <- list(exposure      =expo_mc,
                     param_err     =beta_err_mc,
                     param_ear     =beta_ear_mc,
                     param_err_mort=beta_err_mort_mc,
                     param_ear_mort=beta_ear_mort_mc,
                     wt_transfer   =wt_transfer_mc,
                     ddref         =ddref_mc,
                     lat_t0        =lat_t0_mc,
                     lat_eta       =lat_eta_mc)
    
    ## remove NULL values and
    ## turn list inside-out such that each component
    ## is one parameter set for excess lifetime risk calculation
    l_param1 <- Filter(Negate(is.null), l_param0)
    l_param  <- c(inv_l_param(l_param1))
    
    #####-----------------------------------------------------------------------
    ## get individual excess lifetime risk estimate for each parameter set
    l_out <- if(multicore) {
        stopifnot(n_cores_max > n_cores_omit)
        n_cores <- parallelly::availableCores(logical=TRUE,
                                              max=n_cores_max,
                                              omit=n_cores_omit)

        ## make n_cores batches of l_param
        cl            <- parallel::makeCluster(n_cores)
        l_param_batch <- split(l_param, seq_len(n_sim) %% n_cores)
        
        clusterEvalQ(cl, library(rilear))
        # parallel::clusterExport(cl, c("f_latency", "get_elr1"))
        l_elr <- parallel::parLapply(cl,
                                     l_param_batch,
                                     get_elr1_batch,
                                     lat_method=lat_method,
                                     ...)
        parallel::stopCluster(cl)
        l_elr
    } else {
        lapply(l_param, get_elr1, lat_method=lat_method, ...)
    }
    
    m_elr_mc <- do.call(rbind, l_out)
    
    #####-----------------------------------------------------------------------
    ## summarize excess lifetime risk distribution?
    if(aggr_mc) {
        elr_ci <- apply(m_elr_mc, MARGIN=2, quantile,
                        probs=c(alpha/2, 1 - (alpha/2)), na.rm=TRUE)
        
        elr_mean   <- apply(m_elr_mc, MARGIN=2, mean,   na.rm=TRUE)
        elr_median <- apply(m_elr_mc, MARGIN=2, median, na.rm=TRUE)
        
        rbind(mean_rsk  =elr_mean,
              median_rsk=elr_median,
              CIlo_rsk  =unname(elr_ci[1, , drop=TRUE]),
              CIup_rsk  =unname(elr_ci[2, , drop=TRUE]))
    } else {
        ## otherwise return MC results
        m_elr_mc
    }
}
