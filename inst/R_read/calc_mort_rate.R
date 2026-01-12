#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate sex and age-specific mortality rates
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
source("00_global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## calc mort rate per 1 PY
#####---------------------------------------------------------------------------

calc_mort_rate <- function(d_pop,
                           d_mort,
                           region=c("country", "fedstate", "district"),
                           age_end_max=105L,
                           age2joint_country,
                           age2joint_fedstate,
                           age2joint_district) {
    region <- match.arg(region)
    
    if(region == "country") {
        vars_join  <- c("age_f", "age_n", "sex")
        d_pop_use  <- d_pop
        d_mort_use <- d_mort |>
            mutate(age_f=factor(unname(age2joint_country[as.character(age_f)]),
                                levels=unique(unname(age2joint_country))),
                   age_n=if_else(is.na(as.integer(as.character(age_f))),
                                 get_age_mw(as.character(age_f),
                                            end_max=age_end_max,
                                            out="mid"),
                                 as.integer(as.character(age_f)))) |>
            group_by(sex, age_f, age_n) |>
            summarize(deaths=sum(deaths)) |>
            ungroup()
    } else if(region == "fedstate") {
        vars_join  <- c("ags", "age_f", "age_n", "sex")
        d_pop_use  <- d_pop |>
            select(-any_of(c("fedstate", "state", "district"))) |>
            mutate(age_f=factor(unname(age2joint_fedstate[as.character(age_f)]),
                                levels=unique(unname(age2joint_fedstate))),
                   age_n=if_else(is.na(as.integer(as.character(age_f))),
                                 get_age_grp_mw(as.character(age_f),
                                                end_max=age_end_max,
                                                out="mid"),
                                 as.integer(as.character(age_f)))) |>
            group_by(ags, sex, age_f, age_n) |>
            summarize(pop=sum(pop)) |>
            ungroup()
        
        d_mort_use <- d_mort |>
            select(-any_of(c("fedstate", "state", "district")))
        
    } else if(region == "district") {
        vars_join <- c("ags", "age_f", "age_n", "sex")
        d_pop_use <- d_pop |>
            select(-any_of(c("fedstate", "state", "district"))) |>
            mutate(age_f=factor(unname(age2joint_district[as.character(age_f)]),
                                levels=unique(unname(age2joint_district))),
                   age_n=if_else(is.na(as.integer(as.character(age_f))),
                                 get_age_grp_mw(as.character(age_f),
                                                end_max=age_end_max,
                                                out="mid"),
                                 as.integer(as.character(age_f)))) |>
            group_by(ags, sex, age_f, age_n) |>
            summarize(pop=sum(pop)) |>
            ungroup()
        
        d_mort_use <- d_mort |>
            select(-any_of(c("fedstate", "state", "district"))) |>
            mutate(age_f=factor(unname(age2joint_district[as.character(age_f)]),
                                levels=unique(unname(age2joint_district))),
                   age_n=if_else(is.na(as.integer(as.character(age_f))),
                                 get_age_grp_mw(as.character(age_f),
                                                end_max=age_end_max,
                                                out="mid"),
                                 as.integer(as.character(age_f)))) |>
            group_by(ags, sex, age_f, age_n) |>
            summarize(deaths=sum(deaths)) |>
            ungroup()
    }
    
    d_pop_mortL <- d_pop_use |>
        left_join(d_mort_use, by=vars_join) |>
        mutate(rate=deaths / pop)
    
    vars_var <- c("pop", "deaths", "rate")
    vars_id  <- setdiff(names(d_pop_mortL), c("sex", vars_var))
    
    d_pop_mortW <- d_pop_mortL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =vars_id,
                v.names  =vars_var,
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=d_pop_mortW, dL=d_pop_mortL)
}
