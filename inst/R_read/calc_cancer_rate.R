#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calc cancer rate given cancer cases and population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
source("00_global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## function
#####---------------------------------------------------------------------------

calc_cancer_rate <- function(d_cancer, d_pop, age2joint) {
    d_pop_use <- d_pop |>
        ## cancer cases only on national level, so aggregate
        select(-any_of(c("ags", "district"))) |>
        mutate(age_f=factor(unname(age2joint[as.character(age_f)]),
                            levels=unique(unname(age2joint))),
               age_n=if_else(is.na(as.integer(as.character(age_f))),
                             get_age_grp_mw(as.character(age_f),
                                            end_max=105L,
                                            out="mid"),
                             as.integer(as.character(age_f)))) |>
        group_by(sex, age_f, age_n) |>
        summarize(pop=sum(pop)) |>
        ungroup()
    
    dL <- d_cancer |>
        left_join(d_pop_use, by=join_by(sex, age_f, age_n)) |>
        mutate(rate=cases / pop) |>
        select(-cases, -pop)
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("age_f", "age_n"),
                v.names  ="rate",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}
