# rilear
## Background

R package for assessing the lifetime excess absolute risk from radiation exposure. Based on models for excess relative and excess absolute cancer risk for external exposure. Risk measures include LAR / LEAR / CER, REID / REIC, ELR, and RADS. Supports multiple exposure events with acute (high dose rate) or protracted (low dose rate) exposure.

Monte Carlo methods for assessing uncertainty for risk model coefficient estimates, dose distribution, ERR-EAR weights for risk transfer, DDREF, and the latency function.
 
Informed by methods developed for NCI RadRAT and BfS ProZES.

Currently limited to these outcomes: All solid cancer, breast cancer, leukemia / lymphoma.

Input data for Germany included: Baseline cancer rates, mortality rates, population size on different regional levels. Custom input data for other regions can be used.

## Input data

  - Life table (stratified by sex, age group)
  - Baseline cancer risk (stratified by sex, age group)
      - incidence
          - All solid
          - Leukemia
          - Cancer specific
      - mortality
          - All solid
          - Leukemia
          - Cancer specific
  - Population data (stratified by sex, age group)
      - Country
      - Federal State
      - District
  - Risk models: parameter estimates & covariance matrix
      - Incidence
          - All solid cancer
          - Leukemia / Lymphoma
          - Cancer specific
      - Mortality
          - All solid cancer
          - Leukemia / Lymphoma
          - Cancer specific

## Calculations

  - Sommer et al. Radiat Res 2025. DOI: 10.1667/RADE-24-00060.1
  - ProZES: Ulanowski et al. Radiat Environ Biophys 2020. DOI: 10.1007/s00411-020-00866-7
  - RadRAT: Berrington de Gonzalez et al. JRP 2012. DOI: 10.1088/0952-4746/32/3/205
