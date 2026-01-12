#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## interpolation functions for rates, population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
source("00_global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## functions
#####---------------------------------------------------------------------------

## interpolate cancer rate (given up to age group 85+)
## using natural splines (linear extrapolation)
interpol_cancer <- function(x, age=0:100, out=c("L", "W")) {
    out <- match.arg(out)
    
    ## interpolation stratified by sex
    x_f  <- x |> dplyr::filter(sex == "f")
    x_m  <- x |> dplyr::filter(sex == "m")
    sf_f <- splinefun(x_f[["age_n"]], x_f[["rate"]], method="natural")
    sf_m <- splinefun(x_m[["age_n"]], x_m[["rate"]], method="natural")
    
    ## interpolated rates
    rate_f_i <- sf_f(age)
    rate_m_i <- sf_m(age)
    rate_f_i[rate_f_i < 0] <- 0
    rate_m_i[rate_m_i < 0] <- 0
    
    if(out == "L") {
        data.frame(sex  =factor(rep(c("f", "m"), each=length(age)),
                                levels=c("f", "m")),
                   age_n=c(age, age),
                   rate =c(rate_f_i, rate_m_i))
    } else if(out == "W") {
        data.frame(age_n =age,
                   rate_f=rate_f_i,
                   rate_m=rate_m_i)
    }
}

## interpolate mortality rate
interpol_mort_rate <- function(x,
                               region=c("country", "fedstate", "district"),
                               out=c("L", "W"),
                               method=c("natural", "monoH.FC", "hyman"),
                               age=0:100,
                               age_join=50) {
    out    <- match.arg(out)
    region <- match.arg(region)
    method <- match.arg(method)

    ## stratification
    x_spl <- if(region == "country") {
        split(x, ~ sex)
    } else if(region %in% c("fedstate", "district")) {
        split(x, ~ sex + ags)
    }
    
    ## interpolate rates for 1 stratum
    interpol1 <- function(d) {
        sfun   <- splinefun(d[["age_n"]], d[["rate"]], method=method)
        rate_i <- sfun(age)
        ## interpolation may be negative
        rate_i[rate_i < 0] <- 0
        
        ## for older age, use Gompertz model -> log hazard linear in time
        ## ignoring last available age (may be arbitrary category mid)
        d_fit <- d |>
            mutate(log_rate=log(rate),
                   log_rate=if_else(is.finite(log_rate), log_rate, NA_real_)) |>
            filter((age_n >= age_join) & (age_n < max(age_n)))
        
        fit_g  <- lm(log_rate ~ age_n, data=d_fit)
        rate_g <- exp(predict(fit_g, data.frame(age_n=age)))
        
        ## join interpolation with Gompertz prediction
        rate_out <- c(rate_i[age <= age_join],
                      rate_g[age >  age_join])
        
        d_out  <- data.frame(sex  =d[["sex"]][1],
                             age_n=age,
                             rate =rate_out)
        
        if(hasName(d, "ags")) {
            bind_cols(ags=rep(d[["ags"]][1], nrow(d_out)),
                      d_out)
        } else {
            d_out
        }
    }
    
    l_x_spl <- Map(interpol1, x_spl)
    d_iL    <- bind_rows(l_x_spl)
    
    if(out == "L") {
        d_iL
    } else if(out == "W") {
        vars_var <- "rate"
        vars_id  <- setdiff(names(d_iL), c("sex", vars_var))
        d_iL |>
            as.data.frame() |>
            reshape(direction="wide",
                    idvar    =vars_id,
                    v.names  =vars_var,
                    timevar  ="sex",
                    sep      ="_") |>
            tibble::remove_rownames()
    }
}

## interpolate district population
interpol_pop <- function(x, age=0:100, age_end_max=105L,
                         states, states_inv,
                         age100_fedstate,
                         age100_propf_country) {
    
    ## interpolation for 1 stratum
    interpol1 <- function(d) {
        ## interpolate density -> get age interval width
        age_grp_w <- get_age_grp_mw(as.character(d[["age_org_f"]]),
                                    end_max=age_end_max,
                                    out="width")
        
        age_grp_w_last <- 5L   # width of age group 100+
        pop_total      <- sum(d[["pop"]])
        ags2           <- substr(d[["ags"]][1], 1, 2)
        state_name     <- states[ags2]
        state_abbr     <- unname(states_inv[state_name])
        
        ## number of people age >= 100 per 10000
        age_geq100   <- unname(age100_fedstate[state_abbr])
        n_age_geq100 <- c("f"=   age100_propf_country *age_geq100*pop_total / 10000,
                          "m"=(1-age100_propf_country)*age_geq100*pop_total / 10000)
        
        d_dens_1st  <- d |> mutate(pop_dens=(pop / pop_total) / age_grp_w)
        d_dens_last <- tail(d_dens_1st, n=1) |>
            mutate(age_f ="100+",
                   age_n   =get_age_mw(age_f, end_max=age_end_max),
                   pop     =round(n_age_geq100[as.character(sex)]),
                   pop_dens=(pop / pop_total) / age_grp_w_last)
        
        d_dens <- bind_rows(d_dens_1st, d_dens_last)
        sfun   <- splinefun(d_dens[["age_n"]],
                            d_dens[["pop_dens"]],
                            method="natural")
        
        d_i <- data.frame(ags  =d[["ags"]][1],
                          sex  =d[["sex"]][1],
                          age_n=age) |>
            ## interpolate proportion
            mutate(pop_dens_i=sfun(age_n),
                   ## make sure density is positive
                   pop_dens_i=if_else(pop_dens_i <= 0,
                                      min(d_dens[["pop_dens"]]),
                                      pop_dens_i),
                   ## normalize density to 1
                   pop_dens  =pop_dens_i / sum(pop_dens_i),
                   pop       =round(pop_dens*pop_total))
        
        stopifnot(all(d_i[["pop"]] >= 0L))
        
        d_i
    }
    
    ## stratified by region, sex
    x_spl  <- split(x, ~ ags + sex)
    x_splL <- Map(interpol1, x_spl)
    
    bind_rows(x_splL)
}
