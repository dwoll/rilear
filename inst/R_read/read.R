#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## read all data sources
##
## TODO
## population weighted mean for highest age category
## cancer incidence C44 non-melanoma skin cancer Bavaria
## off by factor of about 10 relative to RKI rates
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(dplyr)
library(restatis)

## load("../../data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## source data import functions
#####---------------------------------------------------------------------------

source("00_global.R",          encoding="UTF8")
source("read_pop_ger.R",       encoding="UTF8")
source("read_mort_ger.R",      encoding="UTF8")
source("read_cancer_ger.R",    encoding="UTF8")
source("read_lifetable_ger.R", encoding="UTF8")
source("calc_mort_rate.R",     encoding="UTF8")
source("calc_cancer_rate.R",   encoding="UTF8")
source("calc_interpolate.R",   encoding="UTF8")

path_data_in <- "../../data_in/"
# gen_auth_save(database="genesis")
# gen_auth_save(database="regio", use_token=FALSE)

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## data sources
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## population Germany federal states & country 2024
#####---------------------------------------------------------------------------

## federal states
l_pop_ger_fedstate_2024 <- read_pop_ger_fedstate(paste0(path_data_in, 
                                                        "pop_ger_fedstate_2024.xlsx"),
                                                 age_grp=age_grp_pop_fedstate,
                                                 states=states_ags)

d_pop_ger_fedstate_2024W <- l_pop_ger_fedstate_2024[["dW"]]
d_pop_ger_fedstate_2024L <- l_pop_ger_fedstate_2024[["dL"]]

## country
d_pop_ger_country_2024W <- d_pop_ger_fedstate_2024W |>
    group_by(age_f, age_n) |>
    summarize(pop_m=sum(pop_m),
              pop_f=sum(pop_f)) |>
    ungroup()

d_pop_ger_country_2024L <- d_pop_ger_fedstate_2024L |>
    group_by(sex, age_f, age_n) |>
    summarize(pop=sum(pop)) |>
    ungroup()

#####---------------------------------------------------------------------------
## population Germany federal states 2017
#####---------------------------------------------------------------------------

l_pop_ger_fedstate_2017 <- read_pop_ger_fedstate(paste0(path_data_in, 
                                                        "pop_ger_fedstate_2017.xlsx"),
                                                 age_grp=age_grp_pop_fedstate,
                                                 states=states_ags)

d_pop_ger_fedstate_2017W <- l_pop_ger_fedstate_2017[["dW"]]
d_pop_ger_fedstate_2017L <- l_pop_ger_fedstate_2017[["dL"]]

#####---------------------------------------------------------------------------
## population Germany district 2024
#####---------------------------------------------------------------------------

l_pop_ger_district_2024 <- read_pop_ger_district_regio(yr=2024,
                                                       age_grp=age_grp_pop_district,
                                                       age_end_max=105L)

d_pop_ger_district_2024W <- l_pop_ger_district_2024[["dW"]]
d_pop_ger_district_2024L <- l_pop_ger_district_2024[["dL"]]

#####---------------------------------------------------------------------------
## population Germany district 2017
#####---------------------------------------------------------------------------

## has AGS "03241001", "05334002", "10041100"
l_pop_ger_district_2017 <- read_pop_ger_district_regio(2017,
                                                       age_grp=age_grp_pop_district,
                                                       age_end_max=105L)

d_pop_ger_district_2017W <- l_pop_ger_district_2017[["dW"]]
d_pop_ger_district_2017L <- l_pop_ger_district_2017[["dL"]]

#####---------------------------------------------------------------------------
## population Bavaria 2018-2022 (for C44 cancer rates)
#####---------------------------------------------------------------------------

l_pop_by_district_2018 <- read_pop_by_district_regio(2018, age_grp=age_grp_pop_district)
l_pop_by_district_2019 <- read_pop_by_district_regio(2019, age_grp=age_grp_pop_district)
l_pop_by_district_2020 <- read_pop_by_district_regio(2020, age_grp=age_grp_pop_district)
l_pop_by_district_2021 <- read_pop_by_district_regio(2021, age_grp=age_grp_pop_district)
l_pop_by_district_2022 <- read_pop_by_district_regio(2022, age_grp=age_grp_pop_district)

d_pop_by_district_2018W <- l_pop_by_district_2018[["dW"]]
d_pop_by_district_2018L <- l_pop_by_district_2018[["dL"]]

d_pop_by_district_2019W <- l_pop_by_district_2019[["dW"]]
d_pop_by_district_2019L <- l_pop_by_district_2019[["dL"]]

d_pop_by_district_2020W <- l_pop_by_district_2020[["dW"]]
d_pop_by_district_2020L <- l_pop_by_district_2020[["dL"]]

d_pop_by_district_2021W <- l_pop_by_district_2021[["dW"]]
d_pop_by_district_2021L <- l_pop_by_district_2021[["dL"]]

d_pop_by_district_2022W <- l_pop_by_district_2022[["dW"]]
d_pop_by_district_2022L <- l_pop_by_district_2022[["dL"]]

d_pop_by_district_5yW <- bind_rows(d_pop_by_district_2018W |> mutate(year=2018),
                                   d_pop_by_district_2019W |> mutate(year=2019),
                                   d_pop_by_district_2020W |> mutate(year=2020),
                                   d_pop_by_district_2021W |> mutate(year=2021),
                                   d_pop_by_district_2022W |> mutate(year=2022)) |>
    group_by(ags, district, age_f, age_n) |>
    summarize(pop_f=sum(pop_f),
              pop_m=sum(pop_m)) |>
    ungroup()

d_pop_by_district5yL<- bind_rows(d_pop_by_district_2018L |> mutate(year=2018),
                                 d_pop_by_district_2019L |> mutate(year=2019),
                                 d_pop_by_district_2020L |> mutate(year=2020),
                                 d_pop_by_district_2021L |> mutate(year=2021),
                                 d_pop_by_district_2022L |> mutate(year=2022)) |>
    group_by(ags, district, sex, age_f, age_n) |>
    summarize(pop=sum(pop)) |>
    ungroup()

#####---------------------------------------------------------------------------
## projected population Germany 2025 federal state (scenario 2 G2L2W2)
#####---------------------------------------------------------------------------

l_pop_proj_ger_fedstate <- read_pop_ger_proj_fedstate(paste0(path_data_in,
                                                             "pop_ger_proj_fedstate_2025.xlsx"),
                                                      age_grp=age_grp_pop_country,
                                                      states=states_ags)

d_pop_proj_ger_fedstateW <- l_pop_proj_ger_fedstate[["dW"]]
d_pop_proj_ger_fedstateL <- l_pop_proj_ger_fedstate[["dL"]]

#####---------------------------------------------------------------------------
## projected population Germany 2025 country (scenario 2 G2L2W2)
#####---------------------------------------------------------------------------

l_pop_proj_ger_country <- read_pop_ger_proj_country(paste0(path_data_in,
                                                           "pop_ger_proj_country_2025.xlsx"),
                                                    age_grp=age_grp_pop_country)

d_pop_proj_ger_countryW <- l_pop_proj_ger_country[["dW"]]
d_pop_proj_ger_countryL <- l_pop_proj_ger_country[["dL"]]

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## mortality
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## mortality Germany country 2024
#####---------------------------------------------------------------------------

l_mort_ger_country_2024 <- read_mort_ger_country(paste0(path_data_in,
                                                        "mort_ger_country_2024.xlsx"),
                                                 age_grp=age_grp_pop_country)

d_mort_ger_country_2024W <- l_mort_ger_country_2024[["dW"]]
d_mort_ger_country_2024L <- l_mort_ger_country_2024[["dL"]]

#####---------------------------------------------------------------------------
## mortality Germany federal states 2017
#####---------------------------------------------------------------------------

l_mort_ger_fedstate_2017 <- read_mort_ger_fedstate(paste0(path_data_in,
                                                          "mort_ger_fedstate_2017.xlsx"),
                                                   age_grp=age_grp_mort_fedstate,
                                                   age_end_max=105L)

d_mort_ger_fedstate_2017W <- l_mort_ger_fedstate_2017[["dW"]]
d_mort_ger_fedstate_2017L <- l_mort_ger_fedstate_2017[["dL"]]

#####---------------------------------------------------------------------------
## mortality Germany district 2017
#####---------------------------------------------------------------------------

l_mort_ger_district_2017 <- read_mort_ger_district_regio(2017,
                                                         age_grp=age_grp_mort_fedstate,
                                                         age_end_max=105L)

d_mort_ger_district_2017W <- l_mort_ger_district_2017[["dW"]]
d_mort_ger_district_2017L <- l_mort_ger_district_2017[["dL"]]

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## lifetable - no interpolation required
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

d_lifetable_ger_2024f <- read_lifetable_ger(paste0(path_data_in,
                                                   "lifetable_female_ger_2022_24.xlsx"),
                                            sex="f",
                                            age_grp=age_grp_lifetable)

d_lifetable_ger_2024m <- read_lifetable_ger(paste0(path_data_in,
                                                   "lifetable_male_ger_2022_24.xlsx"),
                                            sex="m",
                                            age_grp=age_grp_lifetable)

d_lifetable_ger_2024L <- bind_rows(d_lifetable_ger_2024f,
                                   d_lifetable_ger_2024m)

## reshape to wide with respect to sex
vars_id  <- c("age_f", "age_n")
vars_var <- setdiff(names(d_lifetable_ger_2024L), c("sex", vars_id))

d_lifetable_ger_2024W <- d_lifetable_ger_2024L |>
    as.data.frame() |>
    reshape(direction="wide",
            idvar    =vars_id,
            v.names  =vars_var,
            timevar  ="sex",
            sep      ="_") %>%
    tibble::remove_rownames()

rm(d_lifetable_ger_2024f, d_lifetable_ger_2024m, vars_id, vars_var)

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## mortality rates
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## mortality rates Germany country 2024
#####---------------------------------------------------------------------------

l_mort_rate_ger_country_2024 <- calc_mort_rate(d_pop_ger_country_2024L,
                                               d_mort_ger_country_2024L,
                                               region="country",
                                               age2joint_country=age2joint_country)

d_mort_rate_ger_country_2024W <- l_mort_rate_ger_country_2024[["dW"]]
d_mort_rate_ger_country_2024L <- l_mort_rate_ger_country_2024[["dL"]]

d_mort_rate_ger_country_2024W_i <- interpol_mort_rate(d_mort_rate_ger_country_2024L,
                                                      region="country",
                                                      out="W",
                                                      method="monoH.FC")

d_mort_rate_ger_country_2024L_i <- interpol_mort_rate(d_mort_rate_ger_country_2024L,
                                                      region="country",
                                                      out="L",
                                                      method="monoH.FC")

#####---------------------------------------------------------------------------
## mortality rates Germany federal states 2017
#####---------------------------------------------------------------------------

l_mort_rate_ger_fedstate_2017 <- calc_mort_rate(d_pop_ger_fedstate_2017L,
                                                d_mort_ger_fedstate_2017L,
                                                region="fedstate",
                                                age2joint_fedstate=age2joint_fedstate)

d_mort_rate_ger_fedstate_2017W <- l_mort_rate_ger_fedstate_2017[["dW"]]
d_mort_rate_ger_fedstate_2017L <- l_mort_rate_ger_fedstate_2017[["dL"]]

d_mort_rate_ger_fedstate_2017W_i <- interpol_mort_rate(d_mort_rate_ger_fedstate_2017L,
                                                       region="fedstate",
                                                       out="W",
                                                       method="monoH.FC")

d_mort_rate_ger_fedstate_2017L_i <- interpol_mort_rate(d_mort_rate_ger_fedstate_2017L,
                                                       region="fedstate",
                                                       out="L",
                                                       method="monoH.FC")

#####---------------------------------------------------------------------------
## mortality rates Germany district 2017
#####---------------------------------------------------------------------------

l_mort_rate_ger_district_2017 <- calc_mort_rate(d_pop_ger_district_2017L,
                                                d_mort_ger_district_2017L,
                                                region="district",
                                                age2joint_district=age2joint_district)

d_mort_rate_ger_district_2017W <- l_mort_rate_ger_district_2017[["dW"]]
d_mort_rate_ger_district_2017L <- l_mort_rate_ger_district_2017[["dL"]]

d_mort_rate_ger_district_2017W_i <- interpol_mort_rate(d_mort_rate_ger_district_2017L,
                                                       region="district",
                                                       out="W")

d_mort_rate_ger_district_2017L_i <- interpol_mort_rate(d_mort_rate_ger_district_2017L,
                                                       region="district",
                                                       out="L")

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## cancer rates
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

#####---------------------------------------------------------------------------
## cancer incidence
## all solid cancer C00-C80 without C44
#####---------------------------------------------------------------------------

## original rates
l_cancer_ger_incid_solid <- read_cancer_rki(paste0(path_data_in,
                                                   "cancer_incid_all_solid_ger_2023.xls"),
                                            age_grp=age_grp_cancer_rki, n_ref=1L)

l_cancer_ger_incid_leuk_lymph <- read_cancer_rki(paste0(path_data_in,
                                                        "cancer_incid_leuk_lymph_ger_2023.xls"),
                                                 age_grp=age_grp_cancer_rki, n_ref=1L)

## extract data sets
d_cancer_ger_incid_solidW        <- l_cancer_ger_incid_solid[["dW"]]
d_cancer_ger_incid_solidL        <- l_cancer_ger_incid_solid[["dL"]]
d_cancer_ger_incid_leuk_lymphW   <- l_cancer_ger_incid_leuk_lymph[["dW"]]
d_cancer_ger_incid_leuk_lymphL   <- l_cancer_ger_incid_leuk_lymph[["dL"]]

## interpolated rates
d_cancer_ger_incid_solidL_i      <- interpol_cancer(d_cancer_ger_incid_solidL,      out="L")
d_cancer_ger_incid_solidW_i      <- interpol_cancer(d_cancer_ger_incid_solidL,      out="W")
d_cancer_ger_incid_leuk_lymphL_i <- interpol_cancer(d_cancer_ger_incid_leuk_lymphL, out="L")
d_cancer_ger_incid_leuk_lymphW_i <- interpol_cancer(d_cancer_ger_incid_leuk_lymphL, out="W")

#####---------------------------------------------------------------------------
## cancer incidence C50 female breast cancer RKI
#####---------------------------------------------------------------------------

## original number of cases - summed over 5 years
l_cancer_ger_incid_breast <- read_cancer_rki_c50(paste0(path_data_in,
                                                        "cancer_incid_breast_ger_2023.xls"),
                                                 age_grp=age_grp_cancer_rki)

## extract data sets
d_cancer_ger_incid_breastW <- l_cancer_ger_incid_breast[["dW"]]
d_cancer_ger_incid_breastL <- l_cancer_ger_incid_breast[["dL"]]

## interpolated rates
d_cancer_ger_incid_breastL_i <- interpol_cancer(d_cancer_ger_incid_breastL, out="L")
d_cancer_ger_incid_breastW_i <- interpol_cancer(d_cancer_ger_incid_breastL, out="W")

#####---------------------------------------------------------------------------
## cancer mortality C50 female breast cancer RKI
#####---------------------------------------------------------------------------

## original number of cases - summed over 5 years
l_cancer_ger_mort_breast <- read_cancer_rki_c50(paste0(path_data_in,
                                                       "cancer_mort_breast_ger_2024.xls"),
                                                age_grp=age_grp_cancer_rki)

## extract data sets
d_cancer_ger_mort_breastW <- l_cancer_ger_mort_breast[["dW"]]
d_cancer_ger_mort_breastL <- l_cancer_ger_mort_breast[["dL"]]

## interpolated rates
d_cancer_ger_mort_breastL_i <- interpol_cancer(d_cancer_ger_mort_breastL, out="L")
d_cancer_ger_mort_breastW_i <- interpol_cancer(d_cancer_ger_mort_breastL, out="W")

#####---------------------------------------------------------------------------
## cancer incidence C44 non-melanoma skin cancer RKI
#####---------------------------------------------------------------------------

## original number of cases - summed over 5 years
l_cancer_ger_incid_c44 <- read_cancer_rki_c44(paste0(path_data_in,
                                                     "cancer_incid_c44_rki.csv"),
                                              age_grp=age_grp_cancer_rki)

## extract data sets
d_cancer_ger_incid_c44W <- l_cancer_ger_incid_c44[["dW"]]
d_cancer_ger_incid_c44L <- l_cancer_ger_incid_c44[["dL"]]

## interpolated rates
d_cancer_ger_incid_c44L_i <- interpol_cancer(d_cancer_ger_incid_c44L, out="L")
d_cancer_ger_incid_c44W_i <- interpol_cancer(d_cancer_ger_incid_c44L, out="W")

#####---------------------------------------------------------------------------
## cancer incidence C44 non-melanoma skin cancer Bavaria
#####---------------------------------------------------------------------------

## original number of cases - summed over 5 years
l_cancer_by_incid_cases_c44 <- read_cancer_by_c44(paste0(path_data_in,
                                                         "cancer_incid_C44_BY_2018_2022.xlsx"),
                                                  age_grp=age_grp_cancer_by)

## extract data sets
d_cancer_by_incid_cases_c44W <- l_cancer_by_incid_cases_c44[["dW"]]
d_cancer_by_incid_cases_c44L <- l_cancer_by_incid_cases_c44[["dL"]]

## calculate C44 cancer rate
## get population Bavaria 2018-2022
l_cancer_by_incid_c44 <- calc_cancer_rate(d_cancer_by_incid_cases_c44L, # cases summed over 5 years
                                          d_pop_by_district5yL,         # pop summed over 5 years
                                          age2joint=age2joint_c44)

d_cancer_by_incid_c44W <- l_cancer_by_incid_c44[["dW"]]
d_cancer_by_incid_c44L <- l_cancer_by_incid_c44[["dL"]]

## interpolated rates
d_cancer_by_incid_c44L_i <- interpol_cancer(d_cancer_by_incid_c44L, out="L")
d_cancer_by_incid_c44W_i <- interpol_cancer(d_cancer_by_incid_c44L, out="W")

#####---------------------------------------------------------------------------
## rates for solid cancer + C44
#####---------------------------------------------------------------------------

d_cancer_ger_incid_solid_c44W_i <- d_cancer_ger_incid_solidW_i |>
    rename(rate_solid_f=rate_f,
           rate_solid_m=rate_m) |>
    left_join(d_cancer_ger_incid_c44W_i |> rename(rate_c44_f=rate_f, rate_c44_m=rate_m),
              by="age_n") |>
    mutate(rate_f=rate_solid_f + rate_c44_f,
           rate_m=rate_solid_m + rate_c44_m)

#####---------------------------------------------------------------------------
## cancer mortality
#####---------------------------------------------------------------------------

## original rates
l_cancer_ger_mort_solid <- read_cancer_rki(paste0(path_data_in,
                                                  "cancer_mort_all_solid_ger_2024.xls"),
                                           age_grp=age_grp_cancer_rki, n_ref=1L)

l_cancer_ger_mort_leuk_lymph <- read_cancer_rki(paste0(path_data_in,
                                                       "cancer_mort_leuk_lymph_ger_2024.xls"),
                                                age_grp=age_grp_cancer_rki, n_ref=1L)

## extract data sets
d_cancer_ger_mort_solidW        <- l_cancer_ger_mort_solid[["dW"]]
d_cancer_ger_mort_solidL        <- l_cancer_ger_mort_solid[["dL"]]
d_cancer_ger_mort_leuk_lymphW   <- l_cancer_ger_mort_leuk_lymph[["dW"]]
d_cancer_ger_mort_leuk_lymphL   <- l_cancer_ger_mort_leuk_lymph[["dL"]]

## interpolated rates
d_cancer_ger_mort_solidL_i      <- interpol_cancer(d_cancer_ger_mort_solidL,      out="L")
d_cancer_ger_mort_solidW_i      <- interpol_cancer(d_cancer_ger_mort_solidL,      out="W")
d_cancer_ger_mort_leuk_lymphL_i <- interpol_cancer(d_cancer_ger_mort_leuk_lymphL, out="L")
d_cancer_ger_mort_leuk_lymphW_i <- interpol_cancer(d_cancer_ger_mort_leuk_lymphL, out="W")

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## save
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

datasets      <- ls()
datasets_save <- c(datasets[grepl("^d_", datasets)])

save(list=datasets_save, file="../../data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## check lifetable vs. interpolated mortality rate
#####---------------------------------------------------------------------------

library(ggplot2)

d_cmp <- data.frame(age_n=d_lifetable_gerL$age_n,
                    sex  =d_lifetable_gerL$sex,
                    lt_p =d_lifetable_gerL$p,
                    lt_q =d_lifetable_gerL$q,
                    mr_r =d_mort_rate_ger_country_2024L_i$rate) |>
    group_by(sex) |>
    mutate(lt_s=exp(-cumsum(lt_q)),
           mr_s=exp(-cumsum(mr_r))) |>
    ungroup()

ggplot(d_cmp, aes(x=age_n)) +
    geom_point(aes(y=lt_q, color=sex)) +                 # conditional mort prob based on lt
    geom_point(aes(y=lt_s, color=sex)) +                 # cumulative survival based on lt
    geom_line( aes(y=mr_r, color=sex), linewidth=0.8) +  # mortality rate
    geom_line( aes(y=mr_s, color=sex), linewidth=0.8) +  # cumulative survival based on rate
    theme_bw()
