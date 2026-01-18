#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## convenience function to generate n exposure events
## exported
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

gen_exposure <- function(n,
                         agex,
                         timing,
                         dose_distr=c("fixed",         # param = c(value)
                                      "normal",        # param = c(mean, sd)
                                      "lognormal",     # param = c(gmean, gsd)
                                      "triangular",    # param = c(mode, min, max)
                                      "logtriangular", # param = c(mode, min, max)
                                      "uniform",       # param = c(min, max)
                                      "loguniform"),
                         dose_param,
                         dose_rate=c("acute", "chronic"),
                         ddref=1) {
    dose_distr <- match.arg(dose_distr, several.ok=TRUE)
    dose_rate  <- match.arg(dose_rate,  several.ok=TRUE)
    
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
    
    if(!missing(agex) && missing(timing)) {
        if(length(agex) == 1L) {
            agex <- agex + 0:(n-1)
        } else if(length(agex) < n) {
            agex <- c(vapply(seq_len(n), function(i) {
                agex + (i-1)*(diff(range(agex))+1)
            }, FUN.VALUE=numeric(length(agex))))
        }
        
        lapply(seq_len(n), function(i) {
            list(agex      =agex[i],
                 dose_distr=dose_distr[i],
                 dose_param=dose_param[[i]],
                 dose_rate =dose_rate[i],
                 ddref     =ddref[i])
        })
    } else if(!missing(timing) && missing(agex)) {
        if(length(timing) == 1L) {
            timing <- cumsum(c(0, rep(timing, n)))
        } else if(length(timing) < n) {
            timing <- c(vapply(seq_len(n), function(i) {
                timing - min(timing) + (i-1)*(diff(range(timing))+1)
            }, FUN.VALUE=numeric(length(timing))))
        }
        
        lapply(seq_len(n), function(i) {
            list(timing    =timing[i],
                 dose_distr=dose_distr[i],
                 dose_param=dose_param[[i]],
                 dose_rate =dose_rate[i],
                 ddref     =ddref[i])
        })
    } else {
        stop("Provide either 'agex' or 'timing'.")
    }
}
