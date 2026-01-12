#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## Statistisches Bundesamt - population data
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(readxl)

source("00_global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## read empirical population data
## stratified by federal state, sex, age group
## destatis 12411-0013
#####---------------------------------------------------------------------------

read_pop_ger_fedstate <- function(f, age_grp, states, age_end_max=105L) {
    row_start <- 3
    make_header <- function(x, sep=" ") {
        header_1 <- x |> slice(1) |> make_header_string()
        header_2 <- x |> slice(2) |> make_header_string()
        header_1_locf <- locf_value(header_1)
        header_2_locf <- str_c(locf_value(header_2), "_e")
        header_2_use  <- coalesce(header_2, header_2_locf)
        
        coalesce(str_c(header_1_locf, "_", header_2_use), header_1_locf)
    }
    
    #####-----------------------------------------------------------------------
    ## raw data
    #####-----------------------------------------------------------------------
    
    d0 <- read_xlsx(f, col_names=FALSE, skip=3) |>
        tibble::remove_rownames()
    
    header     <- replace_umlauts(make_header(d0, sep=" "))
    states_inv <- setNames(names(states),  unname(states))
    
    dW0 <- d0 |>
        slice(row_start:n()) |>
        setNames(header) |>
        mutate(Datum=if_else(!is.na(as.Date(Altersjahre, format="%d.%m.%Y")),
                             Altersjahre, NA_character_),
               Datum=as.Date(locf_value(Datum), format="%d.%m.%Y")) |>
        filter(Altersjahre != "Insgesamt",
               is.na(as.Date(Altersjahre, format="%d.%m.%Y")),
               !is.na(Bayern_maennlich)) |>
        select(!matches("_Insgesamt$")) |>
        select(!matches("^.+_e$")) |>
        select(-Datum) |>
        rename(age_f=Altersjahre)
    
    #####-----------------------------------------------------------------------
    ## reshape to long
    #####-----------------------------------------------------------------------
    
    names_var <- names(dW0)[grepl("_(maennlich|weiblich)$", names(dW0))]
    names_id  <- setdiff(names(dW0), names_var)

    dL <- dW0 |>
        as.data.frame() |>
        reshape(direction="long",
                idvar    =names_id,
                varying  =names_var,
                v.names  ="pop",
                timevar  ="state_sex") |>
        tibble::remove_rownames() |>
        mutate(state_sex=names_var[state_sex],
               state    =gsub("^([[:alpha:]-]+)_[[:alpha:]]+$", "\\1", state_sex),
               sex      =gsub("^[[:alpha:]-]+_([[:alpha:]]+)$", "\\1", state_sex),
               sex      =factor(sex,
                                levels=c("weiblich", "maennlich"),
                                labels=c("f", "m")),
               ags=unname(states_inv[state]),
               pop=as.numeric(pop),
               age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp)),
               age_n=if_else(age_f == "90+",
                             get_age_grp_mw(as.character(age_f),
                                            end_max=age_end_max, out="mid"),
                             as.integer(as.character(age_f)))) |>
        select(-state_sex)
    
    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("state", "ags", "age_f", "age_n"),
                v.names  ="pop",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}

#####---------------------------------------------------------------------------
## read population data
## stratified by district, sex, age group
## destatis 12411-0018
#####---------------------------------------------------------------------------

read_pop_ger_district <- function(f, age_grp, age_end_max=105L) {
    row_start <- 4
    make_header <- function(x, sep=" ") {
        header_1 <- x |> slice(1) |> make_header_string()
        header_2 <- x |> slice(2) |> make_header_string2()
        header_1_locf <- locf_value(header_1)
        header_2_locf <- str_c(locf_value(header_2), "_e")
        header_2_use  <- coalesce(header_2, header_2_locf)
        
        coalesce(str_c(header_1_locf, "_", header_2_use), header_1_locf)
    }
    
    #####-----------------------------------------------------------------------
    ## raw data - wide with respect to sex, age group
    #####-----------------------------------------------------------------------
    
    d0 <- read_xlsx(f, col_names=FALSE, skip=3) |>
        tibble::remove_rownames()
    
    header <- replace_umlauts(make_header(d0, sep=" "))
    header[1] <- "ags"
    
    dWW <- d0 |>
        setNames(header) |>
        mutate(date=if_else(!is.na(as.Date(ags, format="%d.%m.%Y")),
                            ags, NA_character_),
               date=as.Date(locf_value(date), format="%d.%m.%Y")) |>
        slice(row_start:n()) |>
        filter(!is.na(Kreise),
               !is.na(as.numeric(`maennlich_unter 3 Jahre`))) |>
        select(!matches("^.+_e$")) |>
        rename(district=Kreise)
    
    #####-----------------------------------------------------------------------
    ## reshape to long with respect to age group
    #####-----------------------------------------------------------------------
    
    names_var_f <- names(dWW)[grepl("^weiblich_.+$",  names(dWW))]
    names_var_m <- names(dWW)[grepl("^maennlich_.+$", names(dWW))]
    names_id    <- setdiff(names(dWW), c(names_var_f, names_var_m))
    dWL <- dWW |>
        as.data.frame() |>
        reshape(direction="long",
                idvar    =names_id,
                varying  =list(names_var_f, names_var_m),
                v.names  =c("pop_f", "pop_m"),
                timevar  ="age_f") |>
        tibble::remove_rownames() |>
        mutate(age_f=factor(age_f,
                            levels=seq_len(length(age_grp)),
                            labels=unname(age_grp))) |>
        select(-date) |>
        mutate(age_n=get_age_grp_mw(as.character(age_f),
                                    end_max=age_end_max,
                                    out="mid")) |>
        arrange(ags, age_f)
    
    #####-----------------------------------------------------------------------
    ## reshape to long with respect to sex, age group
    #####-----------------------------------------------------------------------
    
    names_var_ll <- c("pop_f", "pop_m")
    names_id_ll  <- setdiff(names(dWL), names_var_ll) 
    
    dLL <- dWL |>
        as.data.frame() |>
        reshape(direction="long",
                idvar    =names_id_ll,
                varying  =names_var_ll,
                v.names  ="pop",
                timevar  ="sex") |>
        tibble::remove_rownames() |>
        mutate(pop=as.integer(pop),
               sex=factor(sex, levels=1:2, labels=c("f", "m"))) |>
        arrange(ags, sex, age_f)
    
    list(dW=dWL, dL=dLL)
}

#####---------------------------------------------------------------------------
## read predicted population data
## https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Bevoelkerungsvorausberechnung/_inhalt.html#sprg233978
#####---------------------------------------------------------------------------

## stratified by federal state
read_pop_ger_proj_fedstate <- function(f, age_grp, states) {
    d0 <- read_xlsx(f, skip=4) |>
        tibble::remove_rownames()
    
    header     <- c("state_sex", "age_f", "pop", "e")
    states_inv <- setNames(names(states),  unname(states))
    
    #####-----------------------------------------------------------------------
    ## raw data - long with respect to sex
    #####-----------------------------------------------------------------------

    dL <- d0 |>
        setNames(header) |>
        select(-e) |>
        mutate(state_sex=replace_umlauts(state_sex),
               state=if_else(state_sex %in% states, state_sex, NA_character_),
               state=locf_value(state),
               ags  =unname(states_inv[state]),
               sex  =if_else(state_sex %in% c("maennlich", "weiblich", "Insgesamt"),
                             state_sex, NA_character_),
               sex  =replace_umlauts(locf_value(sex)),
               sex  =factor(sex,
                            levels=c("weiblich", "maennlich", "Insgesamt"),
                            labels=c("f", "m", "total")),
               age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp))) |>
        filter(!grepl("^BEV", state_sex),
               !(sex == "total"),
               !is.na(age_f)) |>
        select(state, ags, sex, age_f, pop) |>
        mutate(pop=1000*as.numeric(pop),  # for projected
               age_n=if_else(age_f == "100+", 100L,
                             as.integer(as.character(age_f)))) |>
        droplevels()
    
    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("state", "ags", "age_f", "age_n"),
                v.names  ="pop",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}

## whole country
read_pop_ger_proj_country <- function(f, age_grp, states) {
    d0 <- read_xlsx(f, skip=4) |>
        tibble::remove_rownames()
    
    header <- c("sex", "age_f", "pop", "e")
    
    #####-----------------------------------------------------------------------
    ## raw data - long with respect to sex
    #####-----------------------------------------------------------------------
    
    dL <- d0 |>
        setNames(header) |>
        select(-e) |>
        mutate(sex=if_else(sex %in% c("männlich", "weiblich", "Insgesamt"),
                           sex, NA_character_),
               sex=replace_umlauts(locf_value(sex)),
               sex=factor(sex,
                          levels=c("weiblich", "maennlich", "Insgesamt"),
                          labels=c("f", "m", "total")),
               age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp))) |>
        filter(!grepl("^BEV", sex),
               !(sex == "total"),
               !is.na(age_f)) |>
        select(sex, age_f, pop) |>
        mutate(pop=1000*as.numeric(pop),  # for projected
               age_n=if_else(age_f == "100+", 100L,
                             as.integer(as.character(age_f)))) |>
        droplevels()
    
    #####-----------------------------------------------------------------------
    ## reshape to wide with respect to sex
    #####-----------------------------------------------------------------------
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("age_f", "age_n"),
                v.names  ="pop",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}

read_pop_ger_district_genesis <- function(yr, age_grp, age_end_max=105L) {
    genesis_out <- gen_table(name     ="12411-0018",
                             database ="genesis",
                             startyear=yr,
                             endyear  =yr,
                             language ="de")
    
    dL <- genesis_out |>
        rename(ags     =`1_variable_attribute_code`,
               district=`1_variable_attribute_label`,
               sex     =`2_variable_attribute_label`,
               age_f   =`3_variable_attribute_label`,
               pop=value) |>
        mutate(sex=factor(sex,
                          levels=c("weiblich", "männlich"),
                          labels=c("f", "m")),
               age_f=factor(age_f,
                            levels=names(age_grp),
                            labels=unname(age_grp)),
               age_n=get_age_grp_mw(as.character(age_f),
                                    end_max=age_end_max,
                                    out="mid"),
               pop  =as.integer(pop)) |>
        filter(age_f != "Insgesamt",
               !is.na(pop)) |>
        select(ags, district, sex, age_f, age_n, pop) |>
        arrange(ags, sex, age_f)
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("ags", "district", "age_f", "age_n"),
                v.names  ="pop",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}

read_pop_ger_district_regio <- function(yr, age_grp, age_end_max=105L) {
    regio_out1 <- gen_table(name="12411-04-02-4",
                            database   ="regio",
                            regionalkey="01*, 02*, 03*, 04*, 05*",
                            startyear  =yr,
                            endyear    =yr,
                            language   ="de")

    regio_out2 <- gen_table(name="12411-04-02-4",
                            database   ="regio",
                            regionalkey="06*, 07*, 08*",
                            startyear  =yr,
                            endyear    =yr,
                            language   ="de")
    
    regio_out3 <- gen_table(name="12411-04-02-4",
                            database   ="regio",
                            regionalkey="09*, 10*, 11*, 12*",
                            startyear  =yr,
                            endyear    =yr,
                            language   ="de")
    
    regio_out4 <- gen_table(name="12411-04-02-4",
                            database   ="regio",
                            regionalkey= "13*, 14*, 15*, 16*",
                            startyear  =yr,
                            endyear    =yr,
                            language  ="de")
    
    make_final <- function(d) {
        d_final <- d |>
            rename(ags     =`1_variable_attribute_code`,
                   district=`1_variable_attribute_label`,
                   sex     =`2_variable_attribute_label`,
                   age_f   =`3_variable_attribute_label`,
                   pop     =value) |>
            filter(sex   != "Insgesamt",
                   age_f != "Insgesamt") |>
            mutate(sex=factor(sex,
                              levels=c("weiblich", "männlich"),
                              labels=c("f", "m")),
                   age_f=factor(age_f,
                                levels=names(age_grp),
                                labels=unname(age_grp)),
                   age_n=if_else(is.na(as.integer(as.character(age_f))),
                                 get_age_grp_mw(as.character(age_f),
                                                end_max=age_end_max,
                                                out="mid"),
                                 as.integer(as.character(age_f)))) |>
            select(ags, district, sex, age_f, age_n, pop) |>
            arrange(ags, sex, age_f)
        
        d_final
    }
    
    l_genesis_final <- Map(make_final,
                           list(regio_out1, regio_out2, regio_out3, regio_out4))
    
    dL <- bind_rows(l_genesis_final) |>
        arrange(ags, sex, age_f) |>
        mutate(pop=as.integer(pop)) |> # for old AGS may be "-"
        filter(nchar(ags) >= 5L,
               !is.na(pop))

    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("ags", "district", "age_f", "age_n"),
                v.names  ="pop",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}

read_pop_by_district_regio <- function(yr, age_grp, age_end_max=105L) {
    regio_out <- gen_table(name="12411-04-02-4",
                           database   ="regio",
                           regionalkey="09*",
                           startyear  =yr,
                           endyear    =yr,
                           language   ="de")
    
    make_final <- function(d) {
        d_final <- d |>
            rename(ags     =`1_variable_attribute_code`,
                   district=`1_variable_attribute_label`,
                   sex     =`2_variable_attribute_label`,
                   age_f   =`3_variable_attribute_label`,
                   pop     =value) |>
            filter((sex != "Insgesamt") & (age_f != "Insgesamt")) |>
            mutate(sex=factor(sex,
                              levels=c("weiblich", "männlich"),
                              labels=c("f", "m")),
                   age_f=factor(age_f,
                                levels=names(age_grp),
                                labels=unname(age_grp)),
                   age_n=if_else(is.na(as.integer(as.character(age_f))),
                                 get_age_grp_mw(as.character(age_f),
                                                end_max=age_end_max,
                                                out="mid"),
                                 as.integer(as.character(age_f)))) |>
            select(ags, district, sex, age_f, age_n, pop) |>
            arrange(ags, sex, age_f)
        
        d_final
    }
    
    d_genesis_final <- make_final(regio_out)
    
    dL <- d_genesis_final |>
        arrange(ags, sex, age_f) |>
        mutate(pop=as.integer(pop)) |> # for old AGS may be "-"
        filter(nchar(ags) >= 5L,
               !is.na(pop))
    
    dW <- dL |>
        as.data.frame() |>
        reshape(direction="wide",
                idvar    =c("ags", "district", "age_f", "age_n"),
                v.names  ="pop",
                timevar  ="sex",
                sep      ="_") |>
        tibble::remove_rownames()
    
    list(dW=dW, dL=dL)
}