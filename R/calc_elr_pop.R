#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate the lifetime excess absolute risk due to radiation exposure
## for a complete population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## get excess lifetime risk for one row of a population data set
## pass through all parameters to get_elr_mc() except for agex and sex
#####---------------------------------------------------------------------------

## here: exposure = list of lists(timing, distr, param)
## in call to get_elr_mc():
## exposure = list of lists(agex, distr, param)
get_elr_pop1 <- function(x, exposure, ...) {
    stopifnot(nrow(x) == 1L)
    
    dots    <- list(...)
    age_max <- dots[["age_max"]]
    metric  <- dots[["metric"]]
    aggr_mc <- dots[["aggr_mc"]]
    n_sim   <- dots[["n_sim"]]
    
    if(x[["age_n"]] > age_max) {
        if(!is.null(aggr_mc) && aggr_mc) {
            cbind(x,
                  metric=metric,
                  mean  =NA_real_,
                  median=NA_real_,
                  CIlo  =NA_real_,
                  CIup  =NA_real_)
        } else {
            cbind(x,
                  metric=metric,
                  t(setNames(rep(NA_real_, n_sim),
                             sprintf("MC%.6d", seq_len(n_sim)))))
        }
    } else {
        expo_event <- lapply(exposure, function(event) {
            event[["agex"]] <- x[["age_n"]] + event[["timing"]]
            event
        })

        elr <- get_elr_mc(exposure=expo_event,
                          sex     =as.character(x[["sex"]]),
                          ...)
        
        d_out <- cbind(x, metric=metric, t(elr))
        if(aggr_mc) {
            cbind(d_out,
                  mean_abs  =round(d_out[["mean_rsk"]]  *d_out[["pop"]]),
                  median_abs=round(d_out[["median_rsk"]]*d_out[["pop"]]),
                  CIlo_abs  =round(d_out[["CIlo_rsk"]]  *d_out[["pop"]]),
                  CIup_abs  =round(d_out[["CIup_rsk"]]  *d_out[["pop"]]))
        } else {
            d_out
        }
    }
}

#####---------------------------------------------------------------------------
## get excess lifetime risk for one batch of population entries
## = multiple rows of a population data set
#####---------------------------------------------------------------------------

get_elr_pop_batch1 <- function(x, ...) {
    x_spl <- split(x, seq_len(nrow(x)))
    lapply(x_spl, get_elr_pop1, ...)
}

#####---------------------------------------------------------------------------
## get excess lifetime risk for complete population data set
## do multicore here instead of in get_elr_mc()
## handle MC results here
## exposure: list of lists(timing, distr, param)
#####---------------------------------------------------------------------------

get_elr_pop <- function(x,
                        exposure,
                        stratify_sex=FALSE,
                        pop_ref     =100000,
                        alpha       =0.05,
                        multicore   =FALSE,
                        n_cores_max =10L,
                        n_cores_omit=2L,
                        ...) {
    ## get excess lifetime risk for each row of the population data set
    l_elr <- if(multicore) {
        n_cores <- parallelly::availableCores(logical=TRUE,
                                              max=n_cores_max,
                                              omit=n_cores_omit)

        ## make n_cores batches of rows from population data set
        cl    <- parallel::makeCluster(n_cores)
        x_spl <- split(x, seq_len(nrow(x)) %% n_cores)
        parallel::clusterEvalQ(cl, library(rilear))

        ## collect all MC results here
        l_elr_batch <- parallel::parLapply(cl,
                                           x_spl,
                                           get_elr_pop_batch1,
                                           exposure =exposure,
                                           multicore=FALSE,
                                           aggr_mc  =FALSE,
                                           ...)
        parallel::stopCluster(cl)
        do.call(c, l_elr_batch)
    } else {
        ## split population by row
        x_spl <- split(x, seq_len(nrow(x)))
        
        ## collect all MC results here
        lapply(x_spl,
               get_elr_pop1,
               exposure =exposure,
               multicore=FALSE,
               aggr_mc  =FALSE,
               ...)
    }
    
    ## combine results into data set for all ages at exposure
    ## rows = age, cols = original vars and MC estimates
    d_elr <- do.call(rbind.data.frame, l_elr)

    ## aggregate MC results for summarizing distribution over whole population
    idx_col_mc <- grepl("^MC[[:digit:]]+$", names(d_elr))
    m_mc       <- data.matrix(d_elr[ , idx_col_mc])
    
    mc_summarize <- function(idx) {
        pop_total  <- sum(d_elr[["pop"]][idx])
        weights    <- d_elr[["pop"]][idx] / pop_total
        m_mc_wt    <- weights*m_mc[idx, ]  ## works because cyclic + col-wise
        elr_rsk    <- colSums(m_mc_wt, na.rm=TRUE)
        elr_rsk_ci <- unname(quantile(elr_rsk, probs=c(alpha/2, 1 - (alpha/2))))
        mean_rsk   <- mean(elr_rsk)
        median_rsk <- median(elr_rsk)
        
        c(pop       =pop_total,
          pop_ref   =pop_ref,
          ## relative
          mean_rsk  =mean_rsk,
          median_rsk=median_rsk,
          CIlo_rsk  =elr_rsk_ci[1],
          CIup_rsk  =elr_rsk_ci[2],
          ## absolute for given population
          mean_abs  =round(pop_total*mean_rsk),
          median_abs=round(pop_total*median_rsk),
          CIlo_abs  =round(pop_total*elr_rsk_ci[1]),
          CIup_abs  =round(pop_total*elr_rsk_ci[2]),
          ## per pop_ref
          mean_ref  =round(pop_ref*mean_rsk),
          median_ref=round(pop_ref*median_rsk),
          CIlo_ref  =round(pop_ref*elr_rsk_ci[1]),
          CIup_ref  =round(pop_ref*elr_rsk_ci[2]))
    }
    
    if(stratify_sex) {
        idx_spl <- split(seq_len(nrow(d_elr)), d_elr[["sex"]])
        m_out   <- vapply(idx_spl, mc_summarize, FUN.VALUE=numeric(14L))
        cbind.data.frame(sex=factor(colnames(m_out)), t(m_out))
    } else {
        mc_summarize(seq_len(nrow(d_elr)))
    }
}
