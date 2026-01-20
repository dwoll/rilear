#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## interpolation functions for rates
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## interpolate cancer rate (given up to age group 85+)
#####---------------------------------------------------------------------------

interpol_cancer_rate <- function(x,
                                 region=c("country", "regional"),
                                 age=0:100,
                                 out=c("L", "W")) {
    out    <- match.arg(out)
    region <- match.arg(region)

    ## stratification
    x_spl <- if(region == "country") {
        split(x, ~ sex)
    } else if(region == "regional") {
        split(x, ~ sex + ags)
    }

    interpol1 <- function(d) {
        sfun   <- splinefun(d[["age_n"]], d[["rate"]], method="natural")
        rate_i <- sfun(age)
        ## interpolation may be negative
        rate_i[rate_i < 0] <- 0
        
        d_out <- data.frame(sex  =d[["sex"]][1],
                            age_n=age,
                            rate =rate_i)
        
        ## federal state / district? -> include AGS identifier
        if(hasName(d, "ags")) {
            bind_cols(ags=rep(d[["ags"]][1], nrow(d_out)), d_out)
        } else {
            d_out
        }
    }
    
    ## interpolate for all strata
    l_x_spl <- Map(interpol1, x_spl)
    d_iL    <- bind_rows(l_x_spl)

    if(out == "L") {
        ## long format
        d_iL
    } else if(out == "W") {
        ## wide format
        vars_var <- "rate"
        vars_id  <- setdiff(names(d_iL), c("sex", vars_var))
        d_out <- d_iL |>
            as.data.frame() |>
            reshape(direction="wide",
                    idvar    =vars_id,
                    v.names  =vars_var,
                    timevar  ="sex",
                    sep      ="_")
        
        rownames(d_out) <- NULL
        d_out
    }
}

#####---------------------------------------------------------------------------
## interpolate mortality rate
#####---------------------------------------------------------------------------

interpol_mort_rate <- function(x,
                               region=c("country", "regional"),
                               method=c("natural", "monoH.FC", "hyman"),
                               age=0:100,
                               age_join=40,
                               out=c("L", "W")) {
    region <- match.arg(region)
    out    <- match.arg(out)
    method <- match.arg(method)

    ## stratification
    x_spl <- if(region == "country") {
        split(x, ~ sex)
    } else if(region == "regional") {
        split(x, ~ sex + ags)
    }
    
    ## interpolated rates for 1 stratum
    interpol1 <- function(d) {
        sfun   <- splinefun(d[["age_n"]], d[["rate"]], method=method)
        rate_i <- sfun(age)
        ## interpolation may be negative
        rate_i[rate_i < 0] <- 0
        
        ## for older age, use Gompertz model -> log hazard linear in time
        d_fit <- d |>
            mutate(log_rate=log(.data$rate),
                   log_rate=if_else(is.finite(.data$log_rate), .data$log_rate, NA_real_)) |>
            dplyr::filter(.data$age_n >= age_join,
                          .data$age_n < max(.data$age_n))
        
        fit_g  <- lm(as.formula("log_rate ~ age_n"), data=d_fit)
        rate_g <- exp(predict(fit_g, data.frame(age_n=age)))
        
        ## join interpolation with Gompertz prediction
        ## TODO
        ## this could be done more smoothly
        rate_out <- c(rate_i[age <= age_join],
                      rate_g[age >  age_join])
        
        d_out <- data.frame(sex  =d[["sex"]][1],
                            age_n=age,
                            rate =rate_out)
        
        ## federal state / district? -> include AGS identifier
        if(hasName(d, "ags")) {
            bind_cols(ags=rep(d[["ags"]][1], nrow(d_out)), d_out)
        } else {
            d_out
        }
    }
    
    ## interpolate for all strata
    l_x_spl <- Map(interpol1, x_spl)
    d_iL    <- bind_rows(l_x_spl)
    
    if(out == "L") {
        d_iL
    } else if(out == "W") {
        vars_var <- "rate"
        vars_id  <- setdiff(names(d_iL), c("sex", vars_var))
        d_out <- d_iL |>
            as.data.frame() |>
            reshape(direction="wide",
                    idvar    =vars_id,
                    v.names  =vars_var,
                    timevar  ="sex",
                    sep      ="_")

        rownames(d_out) <- NULL
        d_out
    }
}
