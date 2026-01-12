#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Statistisches Bundesamt Sterbefälle
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(readxl)

source("00_global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## read mortality federal states
#####---------------------------------------------------------------------------

read_mort_ger_fedstate <- function(f, age_grp, age_end_max=105L) {
    d0 <- read_xlsx(f, skip=5) |>
        tibble::remove_rownames()
    
    header <- c("ags", "state", "age_f",
                "all_total", "all_m", "all_f",
                "deaths_total", "deaths_m", "deaths_f")
    
    #####-----------------------------------------------------------------------
    ## original data in wide format with respect to sex
    #####-----------------------------------------------------------------------

    dW <- d0 |>
        setNames(header) |>
        mutate(ags  =locf_value(ags),
               ags  =sprintf("%.2d", as.numeric(ags)),
               state=locf_value(state)) |>
        select(-all_total, -all_m, -all_f, -deaths_total) |>
        filter(age_f != "Insgesamt",
               !is.na(deaths_m)) |>
        mutate(age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp)),
               age_n=get_age_grp_mw(as.character(age_f),
                                    end_max=age_end_max,
                                    out="mid"),
               deaths_m=as.integer(deaths_m),
               deaths_f=as.integer(deaths_f),
               deaths_m=if_else(is.na(deaths_m), 0L, deaths_m),
               deaths_f=if_else(is.na(deaths_f), 0L, deaths_f)) |>
        droplevels()
    
    #####-----------------------------------------------------------------------
    ## reshape to long
    #####-----------------------------------------------------------------------
    
    dL <- dW |>
        as.data.frame() |>
        reshape(direction="long",
                idvar    =c("ags", "state", "age_f", "age_n"),
                varying  =c("deaths_f", "deaths_m"),
                v.names  ="deaths",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames() |>
        mutate(sex=factor(sex, levels=1:2, labels=c("f", "m"))) |>
        arrange(ags, sex, age_f)
    
    list(dW=dW, dL=dL)
}

#####---------------------------------------------------------------------------
## read mortality whole country, single year
#####---------------------------------------------------------------------------

read_mort_ger_country <- function(f, age_grp) {
    d0 <- read_xlsx(f, skip=4) |>
        tibble::remove_rownames()
    
    header <- c("sex_age", "deaths", "e")
    
    dL <- d0 |>
        setNames(header) |>
        mutate(sex=if_else(sex_age %in% c("männlich", "weiblich", "Insgesamt"),
                           sex_age, NA_character_),
               sex=locf_value(sex),
               sex=factor(sex,
                          levels=c("weiblich", "männlich", "total"),
                          labels=c("f", "m", "total")),
               deaths=as.integer(deaths)) |>
        filter(sex != "total",
               !is.na(deaths)) |>
        rename(age_f=sex_age) |>
        mutate(age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp)),
               age_n=if_else(age_f == "100+",
                             100L,
                             as.integer(as.character(age_f)))) |>
        select(-e) |>
        droplevels()
    
    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("age_f", "age_n"),
                v.names  ="deaths",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}

read_mort_ger_district_regio <- function(yr, age_grp, age_end_max=105L) {
    regio_out1 <- gen_table(name="12613-02-02-4",
                            database   ="regio",
                            regionalkey="01*, 02*, 03*, 04*, 05*, 06*, 07*",
                            startyear  =yr,
                            endyear    =yr,
                            language   ="de")
    
    regio_out2 <- gen_table(name="12613-02-02-4",
                            database   ="regio",
                            regionalkey= "08*, 09*, 10*, 11*, 12*",
                            startyear  =yr,
                            endyear    =yr,
                            language   ="de")
    
    regio_out3 <- gen_table(name="12613-02-02-4",
                            database   ="regio",
                            regionalkey= "13*, 14*, 15*, 16*",
                            startyear  =yr,
                            endyear    =yr,
                            language   ="de")
    
    make_final <- function(d) {
        d |>
            rename(ags     =`1_variable_attribute_code`,
                   district=`1_variable_attribute_label`,
                   country =`2_variable_attribute_label`,
                   sex     =`3_variable_attribute_label`,
                   age_f   =`4_variable_attribute_label`,
                   deaths  =value) |>
            filter(sex     != "Insgesamt",
                   country != "Insgesamt",
                   age_f   != "Insgesamt") |>
            mutate(sex=factor(sex,
                              levels=c("weiblich", "männlich"),
                              labels=c("f", "m")),
                   age_f=factor(age_f,
                                levels=names(age_grp),
                                labels=unname(age_grp)),
                   age_n=get_age_grp_mw(as.character(age_f),
                                        end_max=age_end_max,
                                        out="mid"),
                   ## deaths < 3 are "-"
                   deaths=as.integer(deaths),
                   deaths=if_else(is.na(deaths), 0L, deaths)) |>
            select(ags, district, sex, age_f, age_n, deaths) |>
            arrange(ags, sex, age_f)
    }
    
    l_regio_final <- Map(make_final, list(regio_out1, regio_out2, regio_out3))
    
    dL0 <- bind_rows(l_regio_final) |>
        arrange(ags, sex, age_f) |>
        filter(nchar(ags) >= 5L)
    
    ## remove districts with all missing / 0 deaths
    ags_no_deaths <- dL0 |>
        group_by(ags) |>
        summarize(deaths=sum(deaths)) |>
        ungroup() |>
        filter(deaths == 0L) |>
        pull(ags) |>
        unique()
    
    # ags_no_deaths <-
    # c("03152",    "03156",    "05313",    "05354",    "11001001",
    #   "11002002", "11003003", "11004004", "11005005", "11006006",
    #   "11007007", "11008008", "11009009", "11010010", "11011011",
    #   "11012012", "13001",    "13002",    "13005",    "13006",   
    #   "13051",    "13052",    "13053",    "13054",    "13055",   
    #   "13056",    "13057",    "13058",    "13059",    "13060",   
    #   "13061",    "13062",    "14161",    "14166",    "14167",   
    #   "14171",    "14173",    "14177",    "14178",    "14181",   
    #   "14182",    "14188",    "14191",    "14193",    "14262",   
    #   "14263",    "14264",    "14272",    "14280",    "14284",   
    #   "14285",    "14286",    "14287",    "14290",    "14292",   
    #   "14365",    "14374",    "14375",    "14379",    "14383",   
    #   "14389",    "15101",    "15151",    "15153",    "15154",   
    #   "15159",    "15171",    "15202",    "15256",    "15260",   
    #   "15261",    "15265",    "15266",    "15268",    "15303",   
    #   "15352",    "15355",    "15357",    "15358",    "15362",   
    #   "15363",    "15364",    "15367",    "15369",    "15370")
    
    dL <- dL0 |>
        filter(!(ags %in% ags_no_deaths)) |>
        as.data.frame()
    
    dW <- dL |>
        reshape(direction="wide",
                idvar    =c("ags", "district", "age_f", "age_n"),
                v.names  ="deaths",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}