#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## read cancer rates 
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(readxl)

source("00_global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## read RKI cancer rates per 100,000 persons
## https://www.krebsdaten.de/
#####---------------------------------------------------------------------------

read_cancer_rki <- function(f, age_grp, age_end_max=105L, n_ref=100000L) {
    d0 <- read_xls(f, col_names=FALSE, skip=6) |>
        tibble::remove_rownames()
    
    header    <- c("age_f", "sex", "rate")
    n_ref_rki <- 100000L  # rates reported per 100,000 persons

    #####-----------------------------------------------------------------------
    ## raw data - long with respect to sex
    #####-----------------------------------------------------------------------

    dL <- d0 |>
        magrittr::set_colnames(header) |>
        mutate(age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp)),
               sex=factor(sex,
                          levels=c("weiblich", "männlich"),
                          labels=c("f", "m"))) |>
        mutate(rate=(n_ref / n_ref_rki)*rate,  # convert rate to reference size
               age_n=get_age_grp_mw(as.character(age_f),
                                    end_max=age_end_max,
                                    out="mid"))

    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("age_f", "age_n"),
                v.names  ="rate",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dL=dL, dW=dW)
}

#####---------------------------------------------------------------------------
## read RKI C44 cancer rates per 100,000 persons
## https://www.krebsdaten.de/Krebs/DE/Content/Krebsarten/Nicht-melanotischer-Hautkrebs/nicht-melanotischer-hautkrebs_node.html
#####---------------------------------------------------------------------------

read_cancer_rki_c44 <- function(f, age_grp, age_end_max=105L) {
    n_ref_rki <- 100000
    
    d0 <- read.table(f, header=TRUE, sep=";") |>
        tibble::remove_rownames()

    #####-----------------------------------------------------------------------
    ## raw data - long with respect to sex
    #####-----------------------------------------------------------------------
    
    dL <- d0 |>
        mutate(age_f=factor(age_f,
                            levels=unique(unname(age_grp))),
               sex=factor(trimws(sex, which="both"), levels=c("f", "m"))) |>
        mutate(age_n=get_age_grp_mw(as.character(age_f),
                                    end_max=age_end_max,
                                    out="mid"),
               rate=rate / n_ref_rki)

    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("age_f", "age_n"),
                v.names  ="rate",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dL=dL, dW=dW)
}

#####---------------------------------------------------------------------------
## read RKI C50 female breast cancer rates per 100,000 persons
## https://www.krebsdaten.de/
#####---------------------------------------------------------------------------

read_cancer_rki_c50 <- function(f, age_grp, age_end_max=105L) {
    header    <- c("age_f", "sex", "rate")
    n_ref_rki <- 100000  # rates reported per 100,000 persons
    
    d0 <- read_xls(f, col_names=FALSE, skip=6) |>
        tibble::remove_rownames()
    
    #####-----------------------------------------------------------------------
    ## raw data - only f
    #####-----------------------------------------------------------------------
    
    dL_f <- d0 |>
        magrittr::set_colnames(header) |>
        mutate(age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unique(unname(age_grp))),
               sex=factor(sex,
                          levels=c("weiblich", "männlich"),
                          labels=c("f", "m"))) |>
        mutate(age_n=get_age_grp_mw(as.character(age_f),
                                    end_max=age_end_max,
                                    out="mid"),
               rate=rate / n_ref_rki)
    
    ## add male with 0 rate (for female breast cancer)
    dL_m <- dL_f |>
        mutate(sex=factor("m", levels=c("f", "m")),
               rate=0)
    
    dL <- bind_rows(dL_f, dL_m)
    
    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("age_f", "age_n"),
                v.names  ="rate",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dL=dL, dW=dW)
}

#####---------------------------------------------------------------------------
## read Bavarian C44 cancer cases (collected over 5 years)
## https://www.lgl.bayern.de/gesundheit/krebsregister/auswertung_forschung/datenbank/index.htm
#####---------------------------------------------------------------------------

read_cancer_by_c44 <- function(f, age_grp, age_end_max=105L) {
    header <- c("years", "sex", "age_f", "cases", "star")

    d0 <- read_xlsx(f, sheet="Data", col_names=TRUE, skip=5) |>
        tibble::remove_rownames()
    
    #####-----------------------------------------------------------------------
    ## raw data - long with respect to sex
    #####-----------------------------------------------------------------------
    
    dL <- d0 |>
        magrittr::set_colnames(header) |>
        mutate(age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp)),
               sex=factor(sex,
                          levels=c("F", "M"),
                          labels=c("f", "m")),
               age_n=get_age_grp_mw(as.character(age_f),
                                    end_max=age_end_max,
                                    out="mid")) |>
        filter(!is.na(cases)) |>
        select(-years, -star)
    
    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("age_f", "age_n"),
                v.names  ="cases",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dL=dL, dW=dW)
}
