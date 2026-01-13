#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate excess lifetime risk for whole population
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(rilear)
# load("../../data_in/data_in.Rdata")

#####---------------------------------------------------------------------------
## prepare population data and collect results
#####---------------------------------------------------------------------------

d_pop     <- d_pop_ger_country_2024L
d_pop_spl <- split(d_pop, seq_len(nrow(d_pop)))

#####---------------------------------------------------------------------------
## solid cancer
#####---------------------------------------------------------------------------

## use convenience function to generate list of exposure events
## recycling is done to reach n number of exposures
## years are made consecutive from indicated start
expo_event <- gen_exposure(n     =2,
                           timing=1,
                           distr=c("normal", "normal"),
                           param=list(c(mean=0.5, sd=0.05),
                                      c(mean=0.5, sd=0.05)))

## just one row of population data set (not exported)
## TODO Warnmeldung:
# In data.frame(..., check.names = FALSE) :
#    Zeilennamen wurden in einer short Variablen gefunden und wurden verworfen
rilear:::get_elr_pop1(d_pop_spl[[21]],
                      ##
                      exposure          =expo_event,
                      wt_transfer       =c(ERR=0.5, EAR=0.5),
                      wt_transfer_fixed =TRUE,
                      ddref             =1,
                      ddref_fixed       =TRUE,
                      lat_t0            =5,
                      lat_eta           =6.67,
                      lat_method        ="ProZES",
                      lat_fixed         =TRUE,
                      n_sim             =100L,
                      multicore         =TRUE,
                      ##
                      risk_model        =rm_solid_incid_walsh2021(),
                      risk_model_mort   =rm_solid_mort_sumray(),
                      d_base_cancer     =d_cancer_ger_incid_solidW_i,
                      d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
                      d_base_mort       =d_lifetable_ger_2024W,
                      metric            =c("LEAR", "RADS"),
                      ##
                      age_max           =90,
                      aggr_mc           =TRUE)

rilear:::get_elr_pop1(d_pop_spl[[21]],
                      ##
                      exposure          =expo_event,
                      wt_transfer       =c(ERR=0.5, EAR=0.5),
                      wt_transfer_fixed =TRUE,
                      ddref             =1,
                      ddref_fixed       =TRUE,
                      lat_t0            =5,
                      lat_eta           =6.67,
                      lat_method        ="ProZES",
                      lat_fixed         =TRUE,
                      n_sim             =100L,
                      multicore         =TRUE,
                      ##
                      risk_model        =rm_solid_incid_walsh2021(),
                      risk_model_mort   =rm_solid_mort_sumray(),
                      d_base_cancer     =d_cancer_ger_incid_solidW_i,
                      d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
                      d_base_mort       =d_lifetable_ger_2024W,
                      metric            =c("LEAR", "RADS"),
                      ##
                      age_max           =90,
                      aggr_mc           =FALSE)

get_elr_pop(d_pop_ger_country_2024L,
            # d_pop_proj_ger_countryL,
            # d_pop_ger_district_2024L |> filter(ags == "07133"),
            ##
            exposure          =expo_event,
            wt_transfer       =c(ERR=0.5, EAR=0.5),
            wt_transfer_fixed =TRUE,
            ddref             =1,
            ddref_fixed       =TRUE,
            lat_t0            =5,
            lat_eta           =6.67,
            lat_method        ="ProZES",
            lat_fixed         =TRUE,
            n_sim             =100L,
            ##
            risk_model        =rm_solid_incid_walsh2021(),
            risk_model_mort   =rm_solid_mort_sumray(),
            d_base_cancer     =d_cancer_ger_incid_solidW_i,
            d_base_cancer_mort=d_cancer_ger_mort_solidW_i,
            d_base_mort       =d_lifetable_ger_2024W,
            metric            =c("LEAR", "RADS"),
            age_max           =90,
            ##
            multicore         =FALSE,
            n_cores_max       =10L,
            n_cores_omit      =2L,
            stratify_sex      =TRUE,
            pop_ref           =100000)

#####---------------------------------------------------------------------------
## leukemia / lymphoma
#####---------------------------------------------------------------------------

get_elr_pop(d_pop_ger_country_2024L,
            ##
            exposure          =expo_event,
            wt_transfer       =c(ERR=0, EAR=1),
            wt_transfer_fixed =TRUE,
            ddref             =1,
            ddref_fixed       =TRUE,
            lat_t0            =1.5,
            lat_eta           =7.66,
            lat_method        ="ProZES",
            lat_fixed         =TRUE,
            n_sim             =100L,
            ##
            risk_model        =rm_leuk_incid_walsh2021(),
            d_base_cancer     =d_cancer_ger_incid_leuk_lymphW_i,
            d_base_mort       =d_lifetable_ger_2024W,
            metric            ="LEAR",
            age_max           =90,
            ##
            multicore         =TRUE,
            n_cores_max       =10L,
            stratify_sex      =TRUE,
            pop_ref           =100000)
