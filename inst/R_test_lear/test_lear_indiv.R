#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate excess lifetime risk measures for individual person
## single point estimate
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(rilear)
# load("../../data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## solid incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

## generate sets of parameter settings
## with cancer mortality
rm <- list(all_solid =rm_solid_incid_walsh2021(),
           breast    =rm_breast_incid_walsh2021(),
           leuk_lymph=rm_leuk_incid_walsh2021())

rm_mort <- list(all_solid =rm_solid_mort_sumray(),
                breast    =rm_breast_incid_walsh2021(),
                leuk_lymph=rm_leuk_incid_walsh2021())

base_cancer <- list(all_solid =d_cancer_ger_incid_solidW_i,
                    breast    =d_cancer_ger_incid_breastW_i,
                    leuk_lymph=d_cancer_ger_incid_leuk_lymphW_i)

base_cancer_mort <- list(all_solid =d_cancer_ger_mort_solidW_i,
                         breast    =d_cancer_ger_mort_breastW_i,
                         leuk_lymph=d_cancer_ger_mort_leuk_lymphW_i)

wt_transfer <- list(all_solid =c(ERR=0.5, EAR=0.5),
                    breast    =c(ERR=0.0, EAR=1.0),
                    leuk_lymph=c(ERR=1.0, EAR=0.0))

lat_t0 <- list(all_solid =5,
               breast    =5,
               leuk_lymph=1.5)

lat_eta <- list(all_solid =6,
                breast    =6,
                leuk_lymph=6.75)

## TODO
## when n = 1 and cancer_site = vector -> treat cancer_site as list
expo_event <- gen_exposure(n          =1,
                           sex        ="m",
                           agex       =c(20),
                           dose_distr =c("fixed"),
                           dose_param =0.5,
                           ddref      =1.5,
                           dose_rate  ="acute",
                           cancer_site=list(c("all_solid", "leuk_lymph")))

expo_event <- gen_exposure(n          =2,
                           sex        ="f",
                           agex       =20,
                           dose_distr ="fixed",
                           dose_param =0.5,
                           ddref      =1,
                           dose_rate  ="acute",
                           cancer_site=list("breast",
                                            c("all_solid", "breast")))

expo_event <- gen_exposure(n          =2,
                           sex        ="f",
                           agex       =c(20, 21),
                           dose_distr =c("normal", "fixed"),
                           dose_param =list(c(mean=0.5, sd=0.015), 0.5),
                           ddref      =c(1, 1.5),
                           # dose_rate  ="acute",
                           cancer_site=list("breast",
                                            c("all_solid", "breast")))

p_mc <- gen_param_mc(exposure         =expo_event,
                              n_sim            =10,
                              wt_transfer      =wt_transfer,
                              lat_t0           =lat_t0,
                              lat_eta          =lat_eta,
                              lat_method       ="ProZES",
                              lat_fixed        =FALSE,
                              wt_transfer_fixed=FALSE,
                              ddref_fixed      =FALSE,
                              ##
                              risk_model       =rm,
                              risk_model_mort  =rm_mort)

get_lear_indiv(l_param          =p_mc[[1]],
               risk_model      =rm,
               risk_model_mort =rm_mort,
               base_cancer     =base_cancer,
               base_cancer_mort=base_cancer_mort,
               d_base_mort     =d_lifetable_ger_2024W,
               age_max         =90,
               lat_method      ="ProZES",
               metric          =c("LEAR", "RADS", "ELR"))

## breast agex 20, 500 mGy RadRAT 0.0338
## leukemia agex 20, 500 mGy RadRAT 0.00432
## all agex 20, 500 mGy RadRAT 0.129

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
