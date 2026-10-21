#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## calculate excess lifetime risk measures for individual person
## single point estimate
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(rilear)

#####---------------------------------------------------------------------------
## define exposure event(s)
#####---------------------------------------------------------------------------

expo_event1 <- gen_exposure(n          =1,
                            sex        ="f",
                            agex       =20,
                            dose_distr ="normal",
                            dose_param =c(mean=0.5, sd=0.015),
                            ddref      =1.0,
                            dose_rate  ="acute",
                            cancer_site=list(c("all_solid", "leuk_lymph")))

expo_event2 <- gen_exposure(n          =2,
                            sex        ="f",
                            agex       =c(20, 21),
                            dose_distr =c("normal", "fixed"),
                            dose_param =list(c(mean=0.5, sd=0.015), 0.5),
                            ddref      =c(1.0, 1.5),
                            dose_rate  ="acute",
                            cancer_site=list("breast",
                                             c("all_solid", "breast")))

#####---------------------------------------------------------------------------
## risk models cancer incidence, cancer mortality
#####---------------------------------------------------------------------------

rml <- list(all_solid =rm_solid_incid_walsh2021(),
            breast    =rm_breast_incid_walsh2021(),
            leuk_lymph=rm_leuk_incid_walsh2021())

rml_mort <- list(all_solid =rm_solid_mort_sumray(),
                 breast    =rm_breast_incid_walsh2021(),
                 leuk_lymph=rm_leuk_incid_walsh2021())

#####---------------------------------------------------------------------------
## baseline rates cancer incidence, cancer mortality
#####---------------------------------------------------------------------------

base_cancer <- list(all_solid =d_cancer_ger_incid_solidW_i,        # without C44
                    # all_solid =d_cancer_ger_incid_solid_c44W_i,  # with C44
                    breast    =d_cancer_ger_incid_breastW_i,
                    leuk_lymph=d_cancer_ger_incid_leuk_lymphW_i)

base_cancer_mort <- list(all_solid =d_cancer_ger_mort_solidW_i,    # without C44
                         breast    =d_cancer_ger_mort_breastW_i,
                         leuk_lymph=d_cancer_ger_mort_leuk_lymphW_i)

#####---------------------------------------------------------------------------
## sets of parameter settings (with uncertainty)
#####---------------------------------------------------------------------------

wt_transfer <- list(all_solid =c(ERR=0.5, EAR=0.5),
                    breast    =c(ERR=0.0, EAR=1.0),
                    leuk_lymph=c(ERR=1.0, EAR=0.0))

lat_t0 <- list(all_solid =5,
               breast    =5,
               leuk_lymph=1.5)

lat_eta <- list(all_solid =6,
                breast    =6,
                leuk_lymph=6.75)

p_mc <- gen_param_mc(exposure         =expo_event2,
                     n_sim            =10L,
                     wt_transfer      =wt_transfer,
                     lat_t0           =lat_t0,
                     lat_eta          =lat_eta,
                     lat_method       ="ProZES",
                     lat_fixed        =FALSE,
                     wt_transfer_fixed=FALSE,
                     ddref_fixed      =FALSE,
                     ##
                     risk_model       =rml,
                     risk_model_mort  =rml_mort)

p_mc[[1]]

#####---------------------------------------------------------------------------
## LEAR for one set of parameters
#####---------------------------------------------------------------------------

get_lear_indiv(l_param         =p_mc[[1]],
               risk_model      =rml,
               risk_model_mort =rml_mort,
               base_cancer     =base_cancer,
               base_cancer_mort=base_cancer_mort,
               d_base_mort     =d_lifetable_ger_2024W,
               age_max         =90,
               lat_method      ="ProZES",
               metric          =c("LEAR", "RADS", "ELR"))
