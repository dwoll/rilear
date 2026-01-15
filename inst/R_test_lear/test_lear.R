#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate excess lifetime risk measures for individual person
## single point estimate
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(rilear)
# load("../../data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## use convenience function to generate list of exposure events
## recycling is done to reach n number of exposures
## years are made consecutive from indicated start
#####---------------------------------------------------------------------------

expo_event <- gen_exposure(n         =3,
                           agex      =20,
                           dose_distr=c("normal", "triangular"),
                           dose_param=list(c(mean=0.5, sd=0.05),
                                           c(mode=0.5, min=0.1, max=0.6)),
                           dose_rate ="acute",
                           ddref     =1)

#####---------------------------------------------------------------------------
## solid incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

get_lear_1(l_param=list(exposure      =list(dose=c(0.5, 0.5),
                                            agex=c(20, 21)),
                        param_err     =rm_incid$err$param,
                        param_ear     =rm_incid$ear$param,
                        param_err_mort=rm_mort$err$param,
                        param_ear_mort=rm_mort$ear$param,
                        wt_transfer   =c(ERR=0.5, EAR=0.5),
                        ddref         =1,
                        lat_t0        =5,
                        lat_eta       =6.25),
           sex               ="f",
           risk_model        =rm_incid,
           risk_model_mort   =rm_mort,
           d_base_cancer     =d_cancer_ger_incid_solidW_i,
           # d_base_cancer     =d_cancer_ger_incid_solid_c44W_i,
           d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
           d_base_mort       =d_lifetable_ger_2024W,
           # d_base_mort       =d_mort_rate_ger_country_2024W_i,
           age_max           =90,
           lat_method        ="ProZES",
           metric            =c("LEAR", "REID", "ELR", "RADS"))

#####---------------------------------------------------------------------------
## solid incidence, mortality rate, Walsh2021 model
#####---------------------------------------------------------------------------

rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

get_lear_1(l_param=list(exposure      =list(dose=0.5, agex=20),
                        param_err     =rm_incid$err$param,
                        param_ear     =rm_incid$ear$param,
                        param_err_mort=rm_mort$err$param,
                        param_ear_mort=rm_mort$ear$param,
                        wt_transfer   =c(ERR=0.5, EAR=0.5),
                        ddref         =1,
                        lat_t0        =5,
                        lat_eta       =6.25),
           sex               ="f",
           risk_model        =rm_incid,
           risk_model_mort   =rm_mort,
           d_base_cancer     =d_cancer_ger_incid_solidW_i,
           # d_base_cancer   =d_cancer_ger_incid_solid_c44W_i,
           d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
           d_base_mort       =d_mort_rate_ger_country_2024W_i,
           age_max           =90,
           lat_method        ="ProZES",
           metric            ="LEAR")

#####---------------------------------------------------------------------------
## solid incidence, lifetable, SUMRAY model
#####---------------------------------------------------------------------------

rm_incid <- rm_solid_incid_sumray()
rm_mort  <- rm_solid_mort_sumray()

get_lear_1(l_param=list(exposure      =list(dose=0.5, agex=20),
                        param_err     =rm_incid$err$param,
                        param_ear     =rm_incid$ear$param,
                        param_err_mort=rm_mort$err$param,
                        param_ear_mort=rm_mort$ear$param,
                        wt_transfer   =c(ERR=0.5, EAR=0.5),
                        ddref         =1,
                        lat_t0        =5,
                        lat_eta       =6.25),
           sex               ="f",
           risk_model        =rm_incid,
           risk_model_mort   =rm_mort,
           d_base_cancer     =d_cancer_ger_incid_solidW_i,
           # d_base_cancer     =d_cancer_ger_incid_solid_c44W_i,
           d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
           d_base_mort       =d_lifetable_ger_2024W,
           age_max           =90,
           lat_method        ="ProZES",
           metric            ="LEAR")

#####---------------------------------------------------------------------------
## breast incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

rm_incid <- rm_breast_incid_walsh2021()

get_lear_1(l_param=list(exposure   =list(dose=0.5, agex=20),
                        param_err  =rm_incid$err$param,
                        param_ear  =rm_incid$ear$param,
                        wt_transfer=c(ERR=0, EAR=1),
                        ddref      =1,
                        lat_t0     =5,
                        lat_eta    =6.25),
           sex          ="f",
           risk_model   =rm_incid,
           d_base_cancer=d_cancer_ger_incid_breastW_i,
           d_base_mort  =d_lifetable_ger_2024W,
           age_max      =90,
           lat_method   ="ProZES",
           metric       ="LEAR")

#####---------------------------------------------------------------------------
## leukemia incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

rm_incid <- rm_leuk_incid_walsh2021()

get_lear_1(l_param=list(exposure   =list(dose=0.5, agex=20),
                        param_err  =rm_incid$err$param,
                        param_ear  =rm_incid$ear$param,
                        wt_transfer=c(ERR=0.5, EAR=0.5),
                        ddref      =1,
                        lat_t0     =1.5,
                        lat_eta    =7.66),
           sex          ="f",
           risk_model   =rm_incid,
           d_base_cancer=d_cancer_ger_incid_leuk_lymphW_i,
           d_base_mort  =d_lifetable_ger_2024W,
           age_max      =90,
           lat_method   ="ProZES",
           metric       ="LEAR")

#####---------------------------------------------------------------------------
## solid mortality, lifetable, SUMRAY model
#####---------------------------------------------------------------------------

rm_incid <- rm_solid_mort_sumray()
rm_mort  <- rm_solid_mort_sumray()

get_lear_1(l_param=list(exposure      =list(dose=0.5,
                                            agex=20),
                        param_err     =rm_incid$err$param,
                        param_ear     =rm_incid$ear$param,
                        param_err_mort=rm_mort$err$param,
                        param_ear_mort=rm_mort$ear$param,
                        wt_transfer   =c(ERR=0.5, EAR=0.5),
                        ddref         =1,
                        lat_t0        =5,
                        lat_eta       =6.25),
           sex               ="f",
           risk_model        =rm_incid,
           risk_model_mort   =rm_mort,
           d_base_cancer     =d_cancer_ger_mort_solidW_i,
           d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
           d_base_mort       =d_lifetable_ger_2024W,
           age_max           =90,
           lat_method        ="ProZES",
           metric            ="LEAR")

#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## compare to LARisk, RadRAT
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(LARisk)
library(dplyr)

life_ger <- d_lifetable_ger_2024W |>
    select(age_n, q_m, q_f) |>
    rename(age=age_n,
           Prob_d_m=q_m,
           Prob_d_f=q_f)

head(life2010)
head(life_ger)

incid_ger_c50 <- d_cancer_ger_incid_breastW_i |>
    mutate(Site="breast") |>
    rename(Age=age_n,
           Rate_m=rate_m,
           Rate_f=rate_f) |>
    select(Site, Age, Rate_m, Rate_f)

head(incid2010)
head(incid_ger_c50)

sites <- setdiff(unique(incid2010$Site), "leukemia")
exposure_breast <- data.frame(sex          ='female',
                              birth        =2000,
                              exposure     =2020,
                              site         ='breast',
                              exposure_rate="acute",
                              dosedist     ='fixedvalue',
                              dose1        =500,
                              dose2        =NA,
                              dose3        =NA)

exposure_solid_f <- bind_rows(lapply(seq_along(sites), function(i) {
        row_i <- exposure_breast
        row_i[["site"]] <- sites[i]
        row_i
    })) |>
    filter(site != "prostate")

exposure_solid_m <- bind_rows(lapply(seq_along(sites), function(i) {
        row_i <- exposure_breast
        row_i[["site"]] <- sites[i]
        row_i[["sex"]]  <- "male"
        row_i
    })) |>
    filter(!(site %in% c("breast", "uterus", "ovary")))

#####---------------------------------------------------------------------------
## all solid cancer
## RadRAT France: 0.129 (with DDREF) female
#####---------------------------------------------------------------------------

LAR(exposure_solid_f,
    basedata=list(life2010, incid2010),
    current =2090,
    DDREF   =FALSE,
    basepy  =1,
    sim     =500,
    ci      =0.95)
## 0.2590

LAR(exposure_solid_m,
    basedata=list(life2010, incid2010),
    current =2090,
    DDREF   =FALSE,
    basepy  =1,
    sim     =500,
    ci      =0.95)
## 0.114

rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

get_lear_1(l_param=list(exposure      =list(dose=c(0.5),
                                            agex=c(20)),
                        param_err     =rm_incid$err$param,
                        param_ear     =rm_incid$ear$param,
                        param_err_mort=rm_mort$err$param,
                        param_ear_mort=rm_mort$ear$param,
                        wt_transfer   =c(ERR=0.5, EAR=0.5),
                        ddref         =1,
                        lat_t0        =5,
                        lat_eta       =6.25),
           sex               ="m",
           risk_model        =rm_incid,
           risk_model_mort   =rm_mort,
           d_base_cancer     =d_cancer_ger_incid_solidW_i,
           # d_base_cancer     =d_cancer_ger_incid_solid_c44W_i,
           d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
           d_base_mort       =d_lifetable_ger_2024W,
           # d_base_mort       =d_mort_rate_ger_country_2024W_i,
           age_max           =95,
           lat_method        ="ProZES",
           metric            ="LEAR")
## 0.1688 f
## 0.1018 m

#####---------------------------------------------------------------------------
## breast cancer
## RadRAT France 0.0338
#####---------------------------------------------------------------------------

LAR(exposure_breast,
    basedata=list(life2010, incid2010),
    current =2090,
    DDREF   =FALSE,
    basepy  =1,
    sim     =500,
    ci      =0.95)
## 0.0375

rm_incid <- rm_breast_incid_walsh2021()

get_lear_1(l_param=list(exposure   =list(dose=0.5, agex=20),
                        param_err  =rm_incid$err$param,
                        param_ear  =rm_incid$ear$param,
                        wt_transfer=c(ERR=0.0, EAR=1.0),
                        ddref      =1,
                        lat_t0     =5,
                        lat_eta    =6.26),
           sex          ="f",
           risk_model   =rm_incid,
           d_base_cancer=d_cancer_ger_incid_breastW_i,
           d_base_mort  =d_lifetable_ger_2024W,
           age_max      =95,
           lat_method   ="ProZES",
           metric       ="LEAR")
## 0.0295

#####---------------------------------------------------------------------------
## leukemia
## RadRAT France 0.00432
#####---------------------------------------------------------------------------

ex_data_leuk <- data.frame(sex          ='female',
                           birth        =2000,
                           exposure     =2020,
                           site         ='leukemia',
                           exposure_rate="acute",
                           dosedist     ='fixedvalue',
                           dose1        =500,
                           dose2        =NA,
                           dose3        =NA)

LAR(ex_data_leuk,
    basedata=list(life2010, incid2010),
    current =2090,
    DDREF   =FALSE,
    basepy  =1,
    sim     =500,
    ci      =0.95)
## 0.0035

rm_incid <- rm_leuk_incid_walsh2021()

get_lear_1(l_param=list(exposure   =list(dose=0.5, agex=20),
                        param_err  =rm_incid$err$param,
                        param_ear  =rm_incid$ear$param,
                        wt_transfer=c(ERR=0.5, EAR=0.5),
                        ddref      =1,
                        lat_t0     =1.5,
                        lat_eta    =7.66),
           sex          ="f",
           risk_model   =rm_incid,
           d_base_cancer=d_cancer_ger_incid_leuk_lymphW_i,
           d_base_mort  =d_lifetable_ger_2024W,
           age_max      =95,
           lat_method   ="ProZES",
           metric       ="LEAR")
## 0.00863
