#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate excess lifetime risk measures for individual person
## single point estimate
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(rilear)
# load("data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## use convenience function to generate list of exposure events
## recycling is done to reach n number of exposures
## years are made consecutive from indicated start
#####---------------------------------------------------------------------------

expo_event <- gen_exposure(n          =2,
                           sex        ="f",
                           agex       =c(20, 21),
                           dose_distr =c("normal", "fixed"),
                           dose_param =list(c(mean=0.5, sd=0.015), 0.5),
                           ddref      =c(1, 1.5),
                           # dose_rate  ="acute",
                           cancer_site=list("breast",
                                            c("all_solid", "leuk_lymph")))

sim_exposure(x=list(sex        ="f",
                    agex       =20,
                    dose_param =0.5,
                    ddref      =1.5,
                    dose_rate  ="acute",
                    cancer_site=c("all_solid", "leuk_lymph")),
             n_sim=10L,
             ddref_fixed=FALSE)

sim_exposure(x=list(sex="f", agex=20, dose_param=0.5),
             n_sim=10L,
             ddref_fixed=FALSE)

## generate sets of parameter settings
## with cancer mortality
rm      <- list(all_solid =rm_solid_incid_walsh2021(),
                breast    =rm_breast_incid_walsh2021(),
                leuk_lymph=rm_leuk_incid_walsh2021())

rm_mort <- list(all_solid =rm_solid_mort_sumray(),
                breast    =rm_solid_incid_sumray(),
                leuk_lymph=rm_solid_mort_sumray())

p_mc <- gen_param_mc(exposure         =expo_event,
                              n_sim            =10L,
                              wt_transfer      =list(all_solid =c(ERR=0.5, EAR=0.5),
                                                     breast    =c(ERR=0.0, EAR=1.0),
                                                     leuk_lymph=c(ERR=1.0, EAR=0.0)),
                              lat_t0           =list(all_solid =5,
                                                     breast    =5,
                                                     leuk_lymph=1.5),
                              lat_eta          =list(all_solid =6,
                                                     breast    =6,
                                                     leuk_lymph=6.75),
                              lat_method       ="ProZES",
                              lat_fixed        =FALSE,
                              wt_transfer_fixed=FALSE,
                              ddref_fixed      =FALSE,
                              ##
                              risk_model       =rm,
                              risk_model_mort  =rm_mort)

#####---------------------------------------------------------------------------
## solid incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

## generate sets of parameter settings
## with cancer mortality
rm      <- list(breast    =rm_breast_incid_walsh2021(),
                all_solid =rm_solid_incid_walsh2021(),
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

get_lear_indiv_mc(exposure         =expo_event,
                  n_sim            =100L,
                  wt_transfer      =wt_transfer,
                  lat_t0           =lat_t0,
                  lat_eta          =lat_eta,
                  lat_method       ="ProZES",
                  lat_fixed        =FALSE,
                  wt_transfer_fixed=FALSE,
                  ddref_fixed      =FALSE,
                  risk_model       =rm,
                  risk_model_mort  =rm_mort,
                  alpha            =0.05,
                  multicore        =FALSE,
                  n_cores_max      =10L,
                  n_cores_omit     =2L,
                  aggr_mc          =TRUE,
                  base_cancer      =base_cancer,
                  base_cancer_mort =base_cancer_mort,
                  d_base_mort      =d_lifetable_ger_2024W,
                  metric           =c("LEAR", "RADS"),
                  age_max          =90)
