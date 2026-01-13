#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate the lifetime excess absolute risk due to radiation exposure
## for a complete population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## get excess lifetime risk for one row of a population data set
## pass through all parameters to get_elr1_mc() except for agex and sex
#####---------------------------------------------------------------------------

## here: exposure = list of lists(timing, distr, param)
## in call to get_elr1_mc():
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
                  mean_rsk  =NA_real_,
                  median_rsk=NA_real_,
                  CIlo_rsk  =NA_real_,
                  CIup_rsk  =NA_real_)
        } else {
            cbind(x,
                  metric=rep(metric, each=n_sim),
                  mc_n  =sprintf("%.6d", rep(seq_len(n_sim), times=length(metric))),
                  value =rep(NA_real_, length(metric)*n_sim))
        }
    } else {
        expo_event <- lapply(exposure, function(event) {
            event[["agex"]] <- x[["age_n"]] + event[["timing"]]
            event
        })

        ## aggr_mc is passed to get_elr1_mc()
        m_elr <- get_elr1_mc(exposure=expo_event,
                             sex     =as.character(x[["sex"]]),
                             ...)
        
        if(aggr_mc) {
            bind_cols(x, metric=metric, t(m_elr))
        } else {
            ## data frame in long format with respect to metric
            bind_cols(x,
                      metric=rep(metric, each=n_sim),
                      mc_n  =sprintf("%.6d", rep(seq_len(n_sim), times=length(metric))),
                      value =c(m_elr))
        }
    }
}

#####---------------------------------------------------------------------------
## get excess lifetime risk for one batch of population entries
## = multiple rows of a population data set
#####---------------------------------------------------------------------------

get_elr_pop_batch1 <- function(x, ...) {
    x_spl <- split(x, seq_len(nrow(x)))
    l_out <- lapply(x_spl, get_elr_pop1, ...)
    bind_rows(l_out)
}

#####---------------------------------------------------------------------------
## get excess lifetime risk for complete population data set
## do multicore here instead of in get_elr1_mc()
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
        
        bind_rows(l_elr_batch)
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
    ## long format with respect to agex, metric, and MC run
    ## prediction intervals / a-posteriori distribution: simulate events
    d_elr <- bind_rows(l_elr) |>
        mutate(cases=rbinom(n=n(), size=pop, prob=value))

    d_pop_sex <- x |>
        group_by(sex) |>
        summarize(pop_sex=sum(pop)) |>
        ungroup()
    
    ## plausi check: aggregate MC runs per year of age, then population-weighted over age
    # d_elr_mean <- d_elr |>
    #     group_by(metric, sex, age_f, age_n) |>
    #     summarize(pop=unique(pop),
    #               elr=mean(value)) |>
    #     ungroup() |>
    #     left_join(d_pop_sex, by="sex") |>
    #     mutate(weight=pop / pop_total,
    #            elr_w=weight*elr) |>
    #     group_by(metric, sex) |>
    #     summarize(elr=sum(elr_w, na.rm=TRUE)) |>
    #     ungroup()
    
    ## aggregate MC results for summarizing distribution over whole population
    d_elr_out <- d_elr |>
        ## population weighting separately for sex
        left_join(d_pop_sex, by="sex") |>
        mutate(weight =pop / pop_sex,
               value_w=weight*value) |>
        ## summarize per MC run - aggregate over age
        group_by(metric, sex, mc_n) |>
        summarize(elr_rsk=sum(value_w, na.rm=TRUE),
                  cases  =sum(cases, na.rm=TRUE)) |>
        ungroup() |>
        ## distribution over MC runs
        group_by(metric, sex) |>
        summarize(mean_rsk  =mean(elr_rsk),
                  median_rsk=median(elr_rsk),
                  CIlo_rsk  =unname(quantile(elr_rsk, probs=(alpha/2))),
                  CIup_rsk  =unname(quantile(elr_rsk, probs=(1-(alpha/2)))),
                  mean_abs  =mean(cases),
                  median_abs=median(cases),
                  PIlo_abs  =unname(quantile(cases,   probs=(alpha/2))),
                  PIup_abs  =unname(quantile(cases,   probs=(1-(alpha/2))))) |>
        ungroup() |>
        left_join(d_pop_sex, by="sex") |>
        mutate(pop_ref=pop_ref,
               ## per pop_ref
               mean_ref  =round((pop_ref/pop_sex)*mean_abs),
               median_ref=round((pop_ref/pop_sex)*median_abs),
               PIlo_ref  =round((pop_ref/pop_sex)*PIlo_abs),
               PIup_ref  =round((pop_ref/pop_sex)*PIup_abs),
               mean_abs  =round(mean_abs),
               median_abs=round(median_abs),
               PIlo_abs  =round(PIlo_abs),
               PIup_abs  =round(PIup_abs))
    
    d_elr_out
}
