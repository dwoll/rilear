#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Calculate the lifetime excess absolute risk due to radiation exposure
## metrics: LAR / LEAR / CER, REID / REIC, ELR, RADS
## considering uncertainty
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

get_lear1_batch <- function(x, ...) {
    l_out <- lapply(x, get_lear1, ...)
    bind_rows(l_out, .id="id_mc_in")
}

get_lear1_mc <- function(## parameters without uncertainty
                         exposure,
                         sex              =c("f", "m"),
                         n_sim            =1000L,
                         wt_transfer,
                         lat_t0,
                         lat_eta,
                         lat_fixed        =FALSE,
                         wt_transfer_fixed=FALSE,
                         ddref_fixed      =FALSE,
                         metric           =c("LAR", "LEAR", "CER", "REID", "REIC", "ELR", "RADS"),
                         alpha            =0.05,
                         multicore        =FALSE,
                         n_cores_max      =10L,
                         n_cores_omit     =2L,
                         aggr_mc          =TRUE,
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
  
  l_param0 <- gen_param_mc(n_sim            =n_sim,
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

  ## update age at exposure to given row of pop
  l_param <- lapply(l_param0, function(p) {
      p[["exposure"]][["agex"]] <- p[["exposure"]][["agex_timing"]]
      p
  })
  
  ## for each set of parameters
  ## get lifetime excess risk estimates for whole population
  d_learW <- if(multicore) {
    n_cores <- parallelly::availableCores(logical=TRUE,
                                          max=n_cores_max,
                                          omit=n_cores_omit)
    
    ## make n_cores batches of rows from population data set
    cl <- parallel::makeCluster(n_cores)
    l_param_spl <- split(l_param, seq_along(l_param) %% n_cores)
    parallel::clusterExport(cl, "get_lear1")
    parallel::clusterEvalQ(cl, library(rilear))
    
    ## collect all MC results here
    l_lear_batch <- parLapply(cl,
                              l_param_spl,
                              get_lear1_batch,
                              sex               =sex,
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
                       get_lear1,
                       sex               =sex,
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
  
  metric_arg <- metric
  d_learL <- d_learW |>
    as.data.frame() |>
    reshape(direction="long",
            idvar="id_mc",
            varying=metric,
            v.names="value",
            timevar="metric") |>
    mutate(metric=factor(metric, levels=seq_along(metric_arg), labels=metric_arg))
  
  rownames(d_learL) <- NULL
  
    ## aggregate MC results for summarizing distribution
    if(aggr_mc) {
        d_learL |>
            group_by(metric) |>
            summarize(mean_rsk  =mean(value),
                      median_rsk=median(value),
                      CIlo_rsk  =unname(quantile(value, probs=(alpha/2))),
                      CIup_rsk  =unname(quantile(value, probs=(1-(alpha/2))))) |>
            ungroup()
    } else {
        d_learL
    }
}
