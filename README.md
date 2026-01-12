# rilear
## Background

R package for assessing the lifetime excess absolute risk from radiation exposure. Based on models for excess relative and excess absolute cancer risk for external exposure. Risk measures include LAR / LEAR / CER, REID / REIC, ELR, and RADS. Supports multiple exposure events with acute (high dose rate) or protracted (low dose rate) exposure.

Monte Carlo methods for assessing uncertainty for risk model coefficient estimates, dose distribution, ERR-EAR weights for risk transfer, DDREF, and the latency function.
 
Informed by methods developed for NCI RadRAT and BfS ProZES.

Currently limited to these outcomes: All solid cancer, breast cancer, leukemia / lymphoma.

Input data for Germany included: Baseline cancer rates, mortality rates, population size on different regional levels. Custom input data for other regions can be used.

## Input data

  - ERR / EAR risk models: excess risk function, parameter estimates & covariance matrix
  - Baseline life table / overall mortality rates (stratified by sex, age group)
  - Baseline cancer rates (stratified by sex, age group)
  - Baseline cancer mortality rates (stratified by sex, age group) - for REID / REIC, ELR, RADS
  - Population data (stratified by sex, age group)

# Included risk models

  - `rm_solid_incid_walsh2021()`: Solid cancer incidence from Walsh et al. 2021
  - `rm_breast_incid_walsh2021()`: Breast cancer incidence from Walsh et al. 2021
  - `rm_leuk_incid_walsh2021()`: Leukemia / lymphoma incidence from Walsh et al. 2021
  - `rm_solid_incid_sumray()`: Solid cancer incidence from Sasaki et al. 2023
  - `rm_solid_mort_sumray()`: Solid cancer mortality from Sasaki et al. 2023

# Included cancer rates

All stratified by sex, age group. Interpolated for individual years of age.

## Incidence

  - `d_cancer_ger_incid_solidW_i`: Solid cancer incidence Germany - excluding C44 ([RKI](https://www.krebsdaten.de/))
  - `d_cancer_ger_incid_solid_c44W_i`: Solid cancer incidence Germany - including C44 ([RKI](https://www.krebsdaten.de/))
  - `d_cancer_ger_incid_breastW_i`: Breast cancer incidence Germany ([RKI](https://www.krebsdaten.de/))
  - `d_cancer_ger_incid_leuk_lymphW_i`: Leukemia / lymphoma incidence Germany - including CLL (C91.1, C91.4) / ATL (C91.5)([RKI](https://www.krebsdaten.de/))

## Mortality

  - `d_cancer_ger_mort_solidW_i`: Solid cancer mortality Germany- excluding C44 ([RKI](https://www.krebsdaten.de/))
  - `d_cancer_ger_mort_breastW_i`: Breast cancer mortality Germany ([RKI](https://www.krebsdaten.de/))
  - `d_cancer_ger_mort_leuk_lymphW_i`: Leukemia / lymphoma mortality Germany - including CLL (C91.1, C91.4) / ATL (C91.5) ([RKI](https://www.krebsdaten.de/))

# Included population data

All stratified by sex, age group.

## Population

  - `d_pop_ger_country_2024L`: Population Germany 2024 ([destatis](https://www-genesis.destatis.de/))
  - `d_pop_ger_fedstate_2024L`: Population Germany federal states 2024 ([destatis](https://www.regionalstatistik.de/))
  - `d_pop_ger_district_2024L`: Population Germany districts 2024 ([destatis](https://www.regionalstatistik.de/))

## Mortality rates

Interpolated for individual years of age.

  - `d_mort_rate_ger_country_2024W_i`: Mortality rates Germany 2024 ([destatis](https://www-genesis.destatis.de/))
  - `d_mort_rate_ger_fedstate_2017W_i`: Mortality rates Germany federal states 2017 ([destatis](https://www.regionalstatistik.de/))
  - `d_mort_rate_ger_district_2017W_i`: Mortality rates Germany districts 2017 ([destatis](https://www.regionalstatistik.de/))

## Calculations

  - Sommer et al. Radiat Res 2025. DOI: 10.1667/RADE-24-00060.1
  - ProZES: Ulanowski et al. Radiat Environ Biophys 2020. DOI: 10.1007/s00411-020-00866-7
  - RadRAT: Berrington de Gonzalez et al. JRP 2012. DOI: 10.1088/0952-4746/32/3/205
  - Sasaki et al. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213
  - Walsh et al. Radiat Environ Biophys 2019. DOI: 10.1007/s00411-021-00910-0