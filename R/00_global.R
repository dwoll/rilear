#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## internal helper functions
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## general functions
#####---------------------------------------------------------------------------

## map equivalent metric names to unique names
map_metric <- function(x) {
    ## map metrics to unique names
    c(LAR ="LEAR", LEAR="LEAR", CER="LEAR",
      REID="REID", REIC="REID",
      ELR ="ELR",
      RADS="RADS")[toupper(x)] |>
        unname() |>
        unique()
}

## custom length function -> nrow() for matrix, data.frame
get_len <- function(x) {
    if(inherits(x, "matrix") || inherits(x, "data.frame")) {
        nrow(x)
    } else {
        length(x)
    }
}

## custom function to invert / transpose
## turn 2-level parameter list inside-out
inv_l_param <- function(x) {
    comp_names <- names(x)
    comp_lens  <- vapply(x, get_len, FUN.VALUE=numeric(1))
    comp_len   <- unique(comp_lens)
    
    ## lengths of components must be identical
    stopifnot(length(comp_len) == 1L)
    
    swap <- function(i) {
        ## component exposure is already in correct shape
        l_i0 <- lapply(seq_along(x)[-1], function(j) {
            if(inherits(x[[j]], "matrix") ||
               inherits(x[[j]], "data.frame")) {
                x[[j]][i, , drop=TRUE]
            } else {
                x[[j]][[i]]
            }
        })
        
        ## add exposure as first component
        setNames(c(list(x[[1]][[i]]), l_i0), comp_names)
    }
    
    lapply(seq_len(comp_len), swap)
}

## turn 3-level dose list inside out
inv_l_dose <- function(x) {
    n_event   <- length(x)
    n_vars    <- lengths(x)
    var_names <- names(x[[1]])
    n_var     <- unique(n_vars)
    stopifnot(length(unique(n_var)) == 1L)
  
    n_sims <- vapply(x, function(x_i) {
      lengths(x_i)
    }, integer(length(x[[1]])))
  
    n_sim <- unique(c(n_sims))
    stopifnot(length(n_sim) == 1L)
  
    l_out <- lapply(seq_len(n_sim), function(k) {
        l_k0 <- lapply(seq_len(n_var), function(j) {
            sapply(seq_len(n_event), function(i) {
                x[[i]][[j]][[k]]
            })
        })
    
        setNames(l_k0, var_names)
    })
}

#####---------------------------------------------------------------------------
## S-shaped increasing latency function
## tse: time since exposure
## t0: central value for latency
## eta: width of transition period
## ProZES solid:    t0=5,   eta=6.25
## ProZES leukemia: t0=1.5, eta=7.66
## RadRAT solid:    t0=7.5
## RadRAT thyroid:  t0=5
## RadRAT leukemia: t0=2.25
#####---------------------------------------------------------------------------

f_latency <- function(tse, t0, eta, method=c("ProZES", "RadRAT")) {
    method <- match.arg(method)
    if(method == "ProZES") {
        tse[tse < 0] <- NA_real_  # avoid warnings from NaN
        ## ProZES https://doi.org/10.1007/s00411-020-00866-7
        1 / (1 + exp(-eta * log(tse / t0)))
    } else if(method == "RadRAT") {
        ## https://radiationcalculators.cancer.gov/radrat/diff/
        1 / (1 + exp(-(tse - t0)))
    }
}

#####---------------------------------------------------------------------------
## age functions - convert age-band groups to numeric representation
## TODO
## population-weighted mean of highest age category
#####---------------------------------------------------------------------------

## given integer age groups in the form of integers "0", "1", ..., "99", "100+",
## determine interval midpoint and width
get_age_mw <- function(s, end_max=105L, out=c("mid", "width")) {
    out   <- match.arg(out)
    age_n <- as.numeric(s)
    start <- age_n
    end   <- age_n

    ## determine last interval
    idx_na     <- is.na(start)
    start_last <- start[idx_na]
    end_last   <- end[idx_na]
    if(length(start_last) > 0L) {
        start_last <- as.numeric(gsub("^([[:digit:]]+)\\+$", "\\1", s[idx_na]))
    }
    
    if(length(end_last) > 0L) {
        end_last[] <- end_max
    }
    
    start[idx_na] <- start_last
    end[idx_na]   <- end_last
    width         <- end - start + 1
    mid           <- start + 0.5*width
    
    if(out == "mid") {
        mid
    } else if(out == "width") {
        width
    }
}

## given integer age range "05-14", determine interval midpoint and width
get_age_grp_mw <- function(s, end_max=105L, out=c("mid", "width")) {
    out   <- match.arg(out)
    s_spl <- strsplit(s, "-")
    start <- as.numeric(unlist(Map(head, s_spl, n=1L)))
    end   <- as.numeric(unlist(Map(tail, s_spl, n=1L)))
    
    ## determine last interval
    idx_na     <- is.na(start)
    start_last <- start[idx_na]
    end_last   <- end[idx_na]
    if(length(start_last) > 0L) {
        start_last <- as.numeric(gsub("^([[:digit:]]+)\\+$", "\\1", s[idx_na]))
    }
    
    if(length(end_last) > 0L) {
        end_last[] <- end_max
    }
    
    start[idx_na] <- start_last
    end[idx_na]   <- end_last
    width         <- end - start + 1
    mid           <- start + 0.5*width
    
    if(out == "mid") {
        mid
    } else if(out == "width") {
        width
    }
}
