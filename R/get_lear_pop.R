#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate the lifetime excess absolute risk due to radiation exposure
## for a complete population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## get excess lifetime risk for one row of a population data set
## given a single set of parameters in list l_param
#####---------------------------------------------------------------------------

get_lear1_pop1 <- function(x,
                           l_param,
                           age_max,
                           metric,
                           ## only passed throught to get_lear1()
                           # risk_model,
                           # risk_model_mort,
                           # d_base_cancer,
                           # d_base_cancer_mort,
                           # d_base_mort,
                           # lat_method
                           ...
                           ) {
    stopifnot(nrow(x) == 1L)
    
    if(x[["age_n"]] > age_max) {
        cbind(x,
              metric=metric,
              value =rep(NA_real_, length(metric)))
    } else {
        ## update age at exposure to given row of pop
        l_param[["exposure"]][["agex"]] <- x[["age_n"]] +
                                           l_param[["exposure"]][["agex_timing"]]

        m_elr <- get_lear1(l_param,
                           sex    =as.character(x[["sex"]]),
                           age_max=age_max,
                           metric =metric,
                           ## only passed throught to get_lear1()
                           # risk_model        =risk_model,
                           # risk_model_mort   =risk_model_mort,
                           # d_base_cancer     =d_base_cancer,
                           # d_base_cancer_mort=d_base_cancer_mort,
                           # d_base_mort       =d_base_mort,
                           # lat_method        =lat_method
                           ...)
        
        ## data frame in long format with respect to metric
        bind_cols(x, metric=metric, value=c(m_elr))
    }
}

#####---------------------------------------------------------------------------
## get excess lifetime risk for one set of parameters
## for one complete population
#####---------------------------------------------------------------------------

get_lear1_pop <- function(x, d_pop, ...) {
    ## split population into separate rows and get result for each one
    l_pop_spl <- split(d_pop, seq_len(nrow(d_pop)))
    l_out     <- lapply(l_pop_spl, get_lear1_pop1, l_param=x, ...)
    bind_rows(l_out)
}

#####---------------------------------------------------------------------------
## get excess lifetime risk for one batch of sets of parameters
## for one complete population
#####---------------------------------------------------------------------------

get_lear1_pop_batch <- function(x, ...) {
    l_out <- lapply(x, get_lear1_pop, ...)
    bind_rows(l_out, .id="id_mc_in")
}

#####---------------------------------------------------------------------------
## get excess lifetime risk for one complete population
## considering uncertainty by simulating sets of parameters from distributions
## pass through parameters to get_lear1_pop()
#####---------------------------------------------------------------------------

get_lear_pop <- function(x,        # population
                         n_sim            =1000L,
                         exposure,
                         wt_transfer,
                         lat_t0,
                         lat_eta,
                         lat_fixed        =FALSE,
                         wt_transfer_fixed=FALSE,
                         ddref_fixed      =FALSE,
                         metric           =c("LAR", "LEAR", "CER", "REID", "REIC", "ELR", "RADS"),
                         stratify_sex     =FALSE,
                         pop_ref          =100000,
                         alpha            =0.05,
                         multicore        =FALSE,
                         n_cores_max      =10L,
                         n_cores_omit     =2L,
                         ## passed throught to get_lear1()
                         # age_max,
                         # lat_method       =c("ProZES", "RadRAT"),
                         # d_base_cancer,
                         # d_base_cancer_mort,
                         # d_base_mort,
                         # risk_model,
                         # risk_model_mort,
                         ...) {
    metric <- match.arg(metric, several.ok=TRUE)
    metric <- map_metric(metric)
    
    ## get list of parameter sets
    dots <- list(...)
    lat_method      <- dots[["lat_method"]]
    risk_model      <- dots[["risk_model"]]
    risk_model_mort <- dots[["risk_model_mort"]] # may be NULL
    
    l_param <- gen_param_mc(n_sim            =n_sim,
                            exposure         =exposure,
                            wt_transfer      =wt_transfer,
                            lat_t0           =lat_t0,
                            lat_eta          =lat_eta,
                            lat_method       =lat_method,
                            lat_fixed        =lat_fixed,
                            wt_transfer_fixed=wt_transfer_fixed,
                            ddref_fixed      =ddref_fixed,
                            risk_model       =risk_model,
                            risk_model_mort  =risk_model_mort)
    
    ## for each set of parameters
    ## get lifetime excess risk estimates for whole population
    d_lear0 <- if(multicore) {
        n_cores <- parallelly::availableCores(logical=TRUE,
                                              max=n_cores_max,
                                              omit=n_cores_omit)
        
        ## make n_cores batches of rows from population data set
        cl <- parallel::makeCluster(n_cores)
        l_param_spl <- split(l_param, seq_along(l_param) %% n_cores)
        parallel::clusterExport(cl, c("get_lear1_pop", "get_lear1_pop1", "get_lear1"))
        parallel::clusterEvalQ(cl, library(rilear))
        
        ## collect all MC results here
        l_lear_batch <- parLapply(cl,
                                  l_param_spl,
                                  get_lear1_pop_batch,
                                  d_pop             =x,
                                  metric            =metric,
                                  ##
                                  # age_max           =age_max,
                                  # lat_method        =lat_method,
                                  # risk_model        =risk_model,
                                  # risk_model_mort   =risk_model_mort,
                                  # d_base_cancer     =d_base_cancer,
                                  # d_base_cancer_mort=d_base_cancer_mort,
                                  # d_base_mort       =d_base_mort,
                                  ...)

        parallel::stopCluster(cl)
        bind_rows(l_lear_batch, .id="id_batch") |>
            mutate(id_mc=interaction(id_mc_in, id_batch, drop=TRUE, sep="_")) |>
            dplyr::select(-id_mc_in, id_batch)
    } else {
        l_lear <- lapply(l_param,
                         get_lear1_pop,
                         d_pop             =x,
                         metric            =metric,
                         ##
                         # age_max           =age_max,
                         # lat_method        =lat_method
                         # risk_model        =risk_model,
                         # risk_model_mort   =risk_model_mort,
                         # d_base_cancer     =d_base_cancer,
                         # d_base_cancer_mort=d_base_cancer_mort,
                         # d_base_mort       =d_base_mort,
                         ...)
        
        bind_rows(l_lear, .id="id_mc")
    }

    ## combine results into data set for all MC runs
    ## long format with respect to agex, metric, and MC run
    ## prediction intervals / a-posteriori distribution: simulate events
    d_lear <- d_lear0 |>
        mutate(cases=rbinom(n=n(), size=pop, prob=value))
    
    d_pop_sex <- x |>
        group_by(sex) |>
        summarize(pop_sex=sum(pop)) |>
        ungroup()
    
    pop_total <- sum(x[["pop"]])
    
    ## aggregate MC results for summarizing distribution over whole population
    d_lear_out <- if(stratify_sex) {
        d_lear |>
            ## population weighting separately for sex
            left_join(d_pop_sex, by="sex") |>
            mutate(weight =pop / pop_sex,
                   value_w=weight*value) |>
            ## summarize per MC run - aggregate over age
            group_by(metric, sex, id_mc) |>
            summarize(lear_rsk=sum(value_w, na.rm=TRUE),
                      cases   =sum(cases, na.rm=TRUE)) |>
            ungroup() |>
            ## distribution over MC runs
            group_by(metric, sex) |>
            summarize(mean_rsk  =mean(lear_rsk),
                      median_rsk=median(lear_rsk),
                      CIlo_rsk  =unname(quantile(lear_rsk, probs=(alpha/2))),
                      CIup_rsk  =unname(quantile(lear_rsk, probs=(1-(alpha/2)))),
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
                   PIup_abs  =round(PIup_abs)) |>
            rename(pop=pop_sex)
    } else {
        d_lear |>
            ## population weighting
            mutate(weight =pop / pop_total,
                   value_w=weight*value) |>
            ## summarize per MC run - aggregate over age
            group_by(metric, id_mc) |>
            summarize(lear_rsk=sum(value_w, na.rm=TRUE),
                      cases   =sum(cases, na.rm=TRUE)) |>
            ungroup() |>
            ## distribution over MC runs
            group_by(metric) |>
            summarize(mean_rsk  =mean(lear_rsk),
                      median_rsk=median(lear_rsk),
                      CIlo_rsk  =unname(quantile(lear_rsk, probs=(alpha/2))),
                      CIup_rsk  =unname(quantile(lear_rsk, probs=(1-(alpha/2)))),
                      mean_abs  =mean(cases),
                      median_abs=median(cases),
                      PIlo_abs  =unname(quantile(cases,   probs=(alpha/2))),
                      PIup_abs  =unname(quantile(cases,   probs=(1-(alpha/2))))) |>
            ungroup() |>
            mutate(pop       =pop_total,
                   pop_ref   =pop_ref,
                   ## per pop_ref
                   mean_ref  =round((pop_ref/pop_total)*mean_abs),
                   median_ref=round((pop_ref/pop_total)*median_abs),
                   PIlo_ref  =round((pop_ref/pop_total)*PIlo_abs),
                   PIup_ref  =round((pop_ref/pop_total)*PIup_abs),
                   mean_abs  =round(mean_abs),
                   median_abs=round(median_abs),
                   PIlo_abs  =round(PIlo_abs),
                   PIup_abs  =round(PIup_abs))
    }
    
    d_lear_out
}
