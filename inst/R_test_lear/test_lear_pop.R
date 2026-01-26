#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate excess lifetime risk for whole population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(rilear)
# load("data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## prepare population data generate exposure events
#####---------------------------------------------------------------------------

d_pop     <- d_pop_ger_country_2024L
d_pop_spl <- split(d_pop, seq_len(nrow(d_pop)))

## use convenience function to generate list of exposure events
## recycling is done to reach n number of exposures
## years are made consecutive from indicated start
expo_event <- gen_exposure(n          =2,
                           sex        ="f",
                           timing     =1,
                           dose_distr =c("normal", "fixed"),
                           dose_param =list(c(mean=0.5, sd=0.015), 0.5),
                           ddref      =c(1, 1),
                           # dose_rate  ="acute",
                           cancer_site=list("all_solid",
                                            c("all_solid", "leuk_lymph")))

## generate sets of parameter settings
## with cancer mortality
rm <- list(all_solid =rm_solid_incid_walsh2021(),
           breast    =rm_breast_incid_walsh2021(),
           leuk_lymph=rm_leuk_incid_walsh2021())

rm_mort <- list(all_solid =rm_solid_mort_sumray(),
                breast    =rm_solid_incid_sumray(),
                leuk_lymph=rm_solid_mort_sumray())

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

p_mc <- gen_param_mc(exposure         =expo_event,
                     n_sim            =10L,
                     wt_transfer      =wt_transfer,
                     lat_t0           =lat_t0,
                     lat_eta          =lat_eta,
                     lat_method       ="ProZES",
                     lat_fixed        =FALSE,
                     wt_transfer_fixed=FALSE,
                     ddref_fixed      =FALSE,
                     risk_model       =rm,
                     risk_model_mort  =rm_mort)

## without cancer mortality
p_mc <- gen_param_mc(exposure         =expo_event,
                     n_sim            =10L,
                     wt_transfer      =wt_transfer,
                     lat_t0           =lat_t0,
                     lat_eta          =lat_eta,
                     lat_method       ="ProZES",
                     lat_fixed        =FALSE,
                     wt_transfer_fixed=FALSE,
                     ddref_fixed      =FALSE,
                     risk_model       =rm)

#####---------------------------------------------------------------------------
## solid cancer
#####---------------------------------------------------------------------------

d_pop_frankfurt <- d_pop_ger_district_2024L |>
    dplyr::filter(ags == "06412")

# d_pop_ger_country_2024L

## with cancer mortality
get_lear_pop(x                =d_pop_frankfurt,
             n_sim            =10L,
             exposure         =expo_event,
             wt_transfer      =wt_transfer,
             lat_t0           =lat_t0,
             lat_eta          =lat_eta,
             lat_method       ="ProZES",
             lat_fixed        =FALSE,
             wt_transfer_fixed=FALSE,
             ddref_fixed      =FALSE,
             risk_model       =rm,
             # risk_model_mort  =rm_mort,
             stratify_sex     =TRUE,
             pop_ref          =100000,
             alpha            =0.05,
             multicore        =FALSE,
             n_cores_max      =10L,
             n_cores_omit     =2L,
             base_cancer      =base_cancer,
             # base_cancer_mort =base_cancer_mort,
             d_base_mort      =d_lifetable_ger_2024W,
             metric           =c("LEAR", "RADS"),
             age_max          =90) |>
    as.data.frame()
# |>
#     dplyr::select(metric, sex, pop, everything(),
#                   -median_rsk, -median_abs,
#                   -pop_ref, -median_rsk, -CIlo_rsk, -CIup_rsk,
#                   -median_abs,
#                   -mean_ref, -median_ref, -PIlo_ref, -PIup_ref)

## without cancer mortality
get_lear_pop(x=d_pop_ger_country_2024L,
             n_sim            =100,
             exposure         =expo_event,
             wt_transfer      =c(ERR=0.5, EAR=0.5),
             lat_t0           =5,
             lat_eta          =6,
             lat_method       ="ProZES",
             lat_fixed        =FALSE,
             wt_transfer_fixed=FALSE,
             ddref_fixed      =FALSE,
             ##
             risk_model       =rm_solid_incid_walsh2021(),
             ##
             stratify_sex     =FALSE,
             pop_ref          =10000,
             alpha            =0.05,
             multicore        =TRUE,
             n_cores_max      =10L,
             n_cores_omit     =2L,
             ##
             d_base_cancer    =d_cancer_ger_incid_solidW_i,
             d_base_mort      =d_lifetable_ger_2024W,
             metric           =c("LEAR"),
             ##
             age_max          =90)

#####---------------------------------------------------------------------------
## leukemia / lymphoma
#####---------------------------------------------------------------------------

