#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## simulate radiation dose for exposure events
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## random variates from a - possibly asymmetric - triangular distribution
## based on code by Robert Carnell
## https://github.com/bertcarnell/triangle/blob/master/R/rtriangle.r
#####---------------------------------------------------------------------------

dw_rtri <- function(n, t_mode, t_min, t_max) {
    p_u <- runif(n, min=0, max=1)
    if(t_mode != t_max) {
        i <- (t_min + sqrt(     p_u *(t_max - t_min)*(t_mode - t_min)))  <= t_mode
        j <- (t_max - sqrt((1 - p_u)*(t_max - t_min)*(t_max  - t_mode))) >  t_mode
    } else {
        i <- (t_min + sqrt(     p_u *(t_max - t_min)*(t_mode - t_min)))  <  t_mode
        j <- (t_max - sqrt((1 - p_u)*(t_max - t_min)*(t_max  - t_mode))) >= t_mode
    }
    
    if(any(i)) {
        p_u[i] <- t_min + sqrt(     p_u[i] *(t_max - t_min)*(t_mode - t_min))
    }
    
    if(any(j)) {
        p_u[j] <- t_max - sqrt((1 - p_u[j])*(t_max - t_min)*(t_max  - t_mode))
    }
    
    p_u
}

#####---------------------------------------------------------------------------
## simulate dose
## TODO
## simulate DDREF
## possibly dependent on dose, dose_rate "acute" vs. "chronic"
#####---------------------------------------------------------------------------

## supported dose distributions
## see: gen_exposure()
dose_distr_have <- c("fixed",         # param = c(value)
                     "normal",        # param = c(mean, sd)
                     "lognormal",     # param = c(gmean, gsd)
                     "triangular",    # param = c(mode, min, max)
                     "logtriangular", # param = c(mode, min, max)
                     "uniform",       # param = c(min, max)
                     "loguniform")    # param = c(min, max)

sim_dose <- function(x, n_sim, ddref_fixed=FALSE) {
    if(!hasName(x, "dose_distr")) {
        stop("x must have component 'dose_distr' for the dose distribution")
    }
    
    if(!hasName(x, "dose_param")) {
        stop("x must have component 'dose_param' for distribution parameters")
    }
    
    if(!hasName(x, "agex") && !hasName(x, "timing")) {
        stop("x must have component 'agex' or 'timing' for age at exposure")
    }
    
    if(!hasName(x, "dose_rate")) {
        stop("x must have component 'dose_rate' specifying either 'acute' or 'chronic'")
    }

    if(!hasName(x, "ddref")) {
        stop("x must have component 'ddref' specifying the DDREF")
    }
    
    dose_distr  <- x[["dose_distr"]]
    dose_param  <- x[["dose_param"]]
    dose_rate   <- x[["dose_rate"]]
    ddref       <- x[["ddref"]]
    agex_timing <- if(hasName(x, "age")) { x[["agex"]] } else { x[["timing"]] }
    
    if(!(dose_distr %in% dose_distr_have)) {
        stop(paste("x$dose_distr must be one of", 
                   paste(dose_distr_have, collapse=", ")))
    }
    
    dose_mc <- if(dose_distr == "fixed") {
        rep(dose_param[1], n_sim)
    } else if(dose_distr == "normal") {
        rnorm(n=n_sim, mean=dose_param[1], sd=dose_param[2])
    } else if(dose_distr == "lognormal") {
        rlnorm(n=n_sim, meanlog=log(dose_param[1]), sdlog=log(dose_param[2]))
    } else if(dose_distr == "triangular") {
        t_mode <- dose_param[1]
        t_min  <- dose_param[2]
        t_max  <- dose_param[3]
        stopifnot(all(t_min <= t_mode), all(t_mode <= t_max))
        dw_rtri(n_sim, t_mode=t_mode, t_min=t_min, t_max=t_max)
    } else if(dose_distr == "logtriangular") {
        t_mode <- dose_param[1]
        t_min  <- dose_param[2]
        t_max  <- dose_param[3]
        stopifnot(all(t_min <= t_mode), all(t_mode <= t_max))
        exp(dw_rtri(n_sim, t_mode=log(t_mode), t_min=log(t_min), t_max=log(t_max)))
    } else if(dose_distr == "uniform") {
        runif(n_sim, min=dose_param[1], max=dose_param[2])
    } else if(dose_distr == "loguniform") {
        exp(runif(n_sim, min=log(dose_param[1]), max=log(dose_param[2])))
    }
    
    #####-----------------------------------------------------------------------
    ## DDREF
    ddref_mc <- # if(ddref_fixed) {
        rep(ddref, n_sim)
    # } else {
    #     ## TODO ProZES
    #     ## TODO RadRAT
    #     ## threshold low dose rate UNSCEAR 2003: < 6 mGy/h
    #     ## here: choose DDREF in interval [0.8, 3] (ICRP)
    #     ## assume mean 1.5
    #     ## beta distribution in [0, 1] -> 1.5 corresponds to
    #     ## [0.8, 3]-0.8 / 2.2
    #     ## TODO
    #     ## make sure no DDREF for leukemia
    #     norm_var   <- (0.1 / 0.8)^2
    #     norm_mean  <- (1.5-0.8) / 2.2
    #     beta_alpha <-    norm_mean  * ((norm_mean*(1-norm_mean) / norm_var)-1)
    #     beta_beta  <- (1-norm_mean) * ((norm_mean*(1-norm_mean) / norm_var)-1)
    #     
    #     rbeta(n_sim, shape1=beta_alpha, shape2=beta_beta)*2.2 + 0.8
    # }
    
    #####-----------------------------------------------------------------------
    ## dose rate
    dose_rate_mc <- rep(dose_rate, n_sim)
    
    dose_mc[dose_mc < 0] <- 0
    m_dose_mc <- cbind(dose_mc)
    colnames(m_dose_mc) <- agex_timing
    list(dose=m_dose_mc,
         ddref=ddref_mc,
         dose_rate=dose_rate_mc)
}
