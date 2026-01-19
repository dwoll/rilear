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

expo_event <- gen_exposure(n         =1,
                           agex      =20,
                           dose_distr=c("normal"),
                           dose_param=c(mean=0.1, sd=0.015),
                           dose_rate ="acute",
                           ddref     =1)

#####---------------------------------------------------------------------------
## solid incidence, lifetable, Walsh2021 model
#####---------------------------------------------------------------------------

get_lear1_mc(exposure          =expo_event,
             sex               ="f",
             n_sim             =100L,
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
             alpha             =0.05,
             multicore         =FALSE,
             n_cores_max       =10L,
             n_cores_omit      =2L,
             aggr_mc           =TRUE,
             ##
             d_base_cancer     =d_cancer_ger_incid_solidW_i,
             d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
             d_base_mort       =d_lifetable_ger_2024W,
             metric            =c("LEAR", "RADS"),
             ##
             age_max           =90)

