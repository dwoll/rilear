#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate excess lifetime risk measures for individual person
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(rilear)
# load("../../data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## solid incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

## single point estimate
get_elr1(l_param=list(exposure      =list(dose=c(0.5, 0.5),
                                          agex=c(agex1, agex2)),
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

## use convenience function to generate list of exposure events
## recycling is done to reach n number of exposures
## years are made consecutive from indicated start
expo_event <- gen_exposure(n    =3,
                           agex =20,
                           distr=c("normal", "triangular"),
                           param=list(c(mean=0.5, sd=0.05),
                                      c(mode=0.5, min=0.1, max=0.6)))

## point estimate and uncertainty
get_elr1_mc(## parameters with uncertainty
            # exposure=list(list(agex      =20,
            #                    distr="normal",
            #                    param=c(0.5, 0.1)),
            #               list(agex      =21,
            #                    distr="triangular",
            #                    param=c(0.3, 0.1, 0.7))),
            exposure          =expo_event,
            wt_transfer       =c(ERR=0.5, EAR=0.5),
            wt_transfer_fixed =TRUE,
            ddref             =1,
            ddref_fixed       =TRUE,
            lat_t0            =5,
            lat_eta           =6.25,
            lat_method        ="ProZES",
            lat_fixed         =FALSE,
            ## parameters without uncertainty
            sex               ="f",
            risk_model        =rm_solid_incid_walsh2021(),
            risk_model_mort   =rm_solid_mort_sumray(),
            d_base_cancer     =d_cancer_ger_incid_solidW_i,
            # d_base_cancer     =d_cancer_ger_incid_solid_c44W_i,
            d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
            d_base_mort       =d_lifetable_ger_2024W,
            # d_base_mort       =d_mort_rate_ger_country_2024W_i,
            alpha             =0.05,
            age_max           =90,
            metric            =c("LEAR", "RADS"),
            n_sim             =100L,
            n_cores_max       =10L,
            n_cores_omit      =2L,
            multicore         =FALSE,   # makes sense with >= 100000
            aggr_mc           =TRUE)

#####---------------------------------------------------------------------------
## solid incidence, mortality rate, Walsh2021 model
#####---------------------------------------------------------------------------

rm_incid <- rm_solid_incid_walsh2021()
rm_mort  <- rm_solid_mort_sumray()

## single point estimate
get_elr1(l_param=list(exposure      =list(dose=0.5, agex=20),
                      param_err     =rm_incid$err$param,
                      param_ear     =rm_incid$ear$param,
                      param_err_mort=rm_mort$err$param,
                      param_ear_mort=rm_mort$ear$param,
                      wt_transfer   =c(ERR=0.5, EAR=0.5),
                      ddref         =1,
                      lat_t0        =5,
                      lat_eta       =6.25),
         sex               ="f",
         rm_incid        =rm_incid,
         rm_mort   =rm_mort,
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

## single point estimate
get_elr1(l_param=list(exposure      =list(dose=0.5, agex=20),
                      param_err     =rm_incid$err$param,
                      param_ear     =rm_incid$ear$param,
                      param_err_mort=rm_mort$err$param,
                      param_ear_mort=rm_mort$ear$param,
                      wt_transfer   =c(ERR=0.5, EAR=0.5),
                      ddref         =1,
                      lat_t0        =5,
                      lat_eta       =6.25),
         sex               ="f",
         rm_incid        =rm_incid,
         rm_mort   =rm_mort,
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

## single point estimate
get_elr1(l_param=list(exposure   =list(dose=0.5, agex=20),
                      param_err  =rm_incid$err$param,
                      param_ear  =rm_incid$ear$param,
                      wt_transfer=c(ERR=0, EAR=1),
                      ddref      =1,
                      lat_t0     =5,
                      lat_eta    =6.25),
         sex          ="f",
         rm_incid   =rm_incid,
         d_base_cancer=d_cancer_ger_incid_breastW_i,
         d_base_mort  =d_lifetable_ger_2024W,
         age_max      =90,
         lat_method   ="ProZES",
         metric       ="LEAR")

#####---------------------------------------------------------------------------
## leukemia incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

rm_incid <- rm_leuk_incid_walsh2021()

## single point estimate
get_elr1(l_param=list(exposure   =list(dose=0.5, agex=20),
                      param_err  =rm_incid$err$param,
                      param_ear  =rm_incid$ear$param,
                      wt_transfer=c(ERR=0.5, EAR=0.5),
                      ddref      =1,
                      lat_t0     =1.5,
                      lat_eta    =7.66),
         sex          ="f",
         rm_incid   =rm_incid,
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

## single point estimate
get_elr1(l_param=list(exposure      =list(dose=0.5,
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
         rm_incid        =rm_incid,
         rm_mort   =rm_mort,
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
        row_i <- ex_data_breast
        row_i[["site"]] <- sites[i]
        row_i
    })) |>
    filter(site != "prostate")

exposure_solid_m <- bind_rows(lapply(seq_along(sites), function(i) {
        row_i <- ex_data_breast
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

rm_incid      <- rm_solid_incid_walsh2021()
rm_mort <- rm_solid_mort_sumray()

## single point estimate
get_elr1(l_param=list(exposure      =list(dose=c(0.5),
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
         rm_incid        =rm_incid,
         rm_mort   =rm_mort,
         d_base_cancer     =d_cancer_ger_incid_solidW_i,
         # d_base_cancer     =d_cancer_ger_incid_solid_c44W_i,
         d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
         d_base_mort       =d_lifetable_ger_2024W,
         # d_base_mort       =d_mort_rate_ger_country_2024W_i,
         age_max           =95,
         lat_method        ="ProZES",
         metric            ="LEAR")
## 0.1734 f
## 0.1056 m

#####---------------------------------------------------------------------------
## breast cancer
## RadRAT France 0.0338
#####---------------------------------------------------------------------------

LAR(ex_data_breast,
    basedata=list(life2010, incid2010),
    current =2090,
    DDREF   =FALSE,
    basepy  =1,
    sim     =500,
    ci      =0.95)
## 0.0375

## single point estimate
rm_incid <- rm_breast_incid_walsh2021()
get_elr1(l_param=list(exposure   =list(dose=0.5, agex=20),
                      param_err  =rm_incid$err$param,
                      param_ear  =rm_incid$ear$param,
                      wt_transfer=c(ERR=0.0, EAR=1.0),
                      ddref      =1,
                      lat_t0     =5,
                      lat_eta    =6.26),
         sex          ="f",
         rm_incid   =rm_incid,
         d_base_cancer=d_cancer_ger_incid_breastW_i,
         d_base_mort  =d_lifetable_ger_2024W,
         age_max      =95,
         lat_method   ="ProZES",
         metric       ="LEAR")
## 0.0304

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

## single point estimate
rm_incid <- rm_leuk_incid_walsh2021()
get_elr1(l_param=list(exposure   =list(dose=0.5, agex=20),
                      param_err  =rm_incid$err$param,
                      param_ear  =rm_incid$ear$param,
                      wt_transfer=c(ERR=0.5, EAR=0.5),
                      ddref      =1,
                      lat_t0     =1.5,
                      lat_eta    =7.66),
         sex          ="f",
         rm_incid   =rm_incid,
         d_base_cancer=d_cancer_ger_incid_leuk_lymphW_i,
         d_base_mort  =d_lifetable_ger_2024W,
         age_max      =95,
         lat_method   ="ProZES",
         metric       ="LEAR")
## 0.008757