#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## convenience function to generate n exposure events
## exported
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

gen_exposure <- function(n,
                         sex        =c("f", "m"),
                         agex,    # for exposed individuals: age at exposure
                         timing,  # for exposed population: interval of exposure events
                         dose_distr ="fixed",
                         dose_param,
                         dose_rate  ="acute",
                         ddref      =1,
                         cancer_site="all_solid") {
    sex <- match.arg(sex)
    
    ## use strict matching for dose_distr, dose_rate, cancer_site
    dose_distr  <- check_arg(dose_distr,
                             values=dose_distr_have,
                             multiple=TRUE)
    
    dose_rate   <- check_arg(dose_rate,
                             values=c("acute", "chronic"),
                             multiple=TRUE)
    
    l_cancer_site <- lapply(cancer_site,
                            check_arg,
                            values=cancer_sites_have,
                            multiple=TRUE)
    
    if(!inherits(dose_param, "list")) {
        dose_param <- rep(list(dose_param), n)
    } else if(inherits(dose_param, "list") && (length(dose_param) < n)) {
        dose_param <- rep(dose_param, n)
    }
    
    if(length(dose_distr) < n) {
        dose_distr <- rep(dose_distr, n)
    }
    
    if(length(dose_rate) < n) {
        dose_rate <- rep(dose_rate, n)
    }
    
    if(length(ddref) < n) {
        ddref <- rep(ddref, n)
    }

    if(length(l_cancer_site) < n) {
        l_cancer_site <- rep(l_cancer_site, n)
    }
    
    ## have agex - for exposure of individual
    if(!missing(agex) && missing(timing)) {
        ## assume subsequent years of exposure
        if(length(agex) == 1L) {
            agex <- agex + 0:(n-1)
        } else if(length(agex) < n) {
            agex <- c(vapply(seq_len(n), function(i) {
                agex + (i-1)*(diff(range(agex))+1)
            }, FUN.VALUE=numeric(length(agex))))
        }
        
        lapply(seq_len(n), function(i) {
            list(sex        =sex,
                 agex       =agex[i],
                 dose_distr =dose_distr[i],
                 dose_param =dose_param[[i]],
                 dose_rate  =dose_rate[i],
                 ddref      =ddref[i],
                 cancer_site=l_cancer_site[[i]])
        })
    ## have timing - for exposure of population
    } else if(!missing(timing) && missing(agex)) {
        if(length(timing) == 1L) {
            ## assume subsequent years of exposure
            timing <- cumsum(c(0, rep(timing, n)))
        } else if(length(timing) < n) {
            timing <- c(vapply(seq_len(n), function(i) {
                timing - min(timing) + (i-1)*(diff(range(timing))+1)
            }, FUN.VALUE=numeric(length(timing))))
        }
        
        lapply(seq_len(n), function(i) {
            list(sex        =sex,
                 timing     =timing[i],
                 dose_distr =dose_distr[i],
                 dose_param =dose_param[[i]],
                 dose_rate  =dose_rate[i],
                 ddref      =ddref[i],
                 cancer_site=l_cancer_site[[i]])
        })
    } else {
        stop("Provide either 'agex' or 'timing'.")
    }
}
