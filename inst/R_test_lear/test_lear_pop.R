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
expo_event <- gen_exposure(n     =2,
                           timing=1,
                           # agex =c(20, 21),
                           ddref=c(1, 1.5),
                           dose_rate=c("acute", "chronic"),
                           dose_distr=c("normal", "normal"),
                           dose_param=list(c(mean=0.5, sd=0.05),
                                           c(mean=0.5, sd=0.05)))

## generate sets of parameter settings
## with cancer mortality
rilear:::gen_param_mc(n_sim=10,
                      exposure         =expo_event,
                      wt_transfer      =c(ERR=0.5, EAR=0.5),
                      lat_t0=5,
                      lat_eta=6,
                      lat_method       ="ProZES",
                      lat_fixed        =FALSE,
                      wt_transfer_fixed=FALSE,
                      ddref_fixed      =FALSE,
                      ##
                      risk_model       =rm_solid_incid_walsh2021(),
                      risk_model_mort  =rm_solid_mort_sumray())

## without cancer mortality
rilear:::gen_param_mc(n_sim=10,
                      exposure         =expo_event,
                      wt_transfer      =c(ERR=0.5, EAR=0.5),
                      lat_t0=5,
                      lat_eta=6,
                      lat_method       ="ProZES",
                      lat_fixed        =FALSE,
                      wt_transfer_fixed=FALSE,
                      ddref_fixed      =FALSE,
                      ##
                      risk_model       =rm_solid_incid_walsh2021())

#####---------------------------------------------------------------------------
## solid cancer
#####---------------------------------------------------------------------------

## with cancer mortality
get_lear_pop(x=d_pop_ger_country_2024L,
             n_sim             =100L,
             exposure          =expo_event,
             wt_transfer       =c(ERR=0.5, EAR=0.5),
             lat_t0            =5,
             lat_eta           =6,
             lat_method        ="ProZES",
             lat_fixed         =FALSE,
             wt_transfer_fixed =FALSE,
             ddref_fixed       =FALSE,
             ##
             risk_model        =rm_solid_incid_walsh2021(),
             risk_model_mort   =rm_solid_mort_sumray(),
             ##
             stratify_sex      =FALSE,
             pop_ref           =10000,
             alpha             =0.05,
             multicore         =TRUE,
             n_cores_max       =10L,
             n_cores_omit      =2L,
             ##
             d_base_cancer     =d_cancer_ger_incid_solidW_i,
             d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
             d_base_mort       =d_lifetable_ger_2024W,
             metric            =c("lear", "RADS"),
             ##
             age_max           =90)

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

