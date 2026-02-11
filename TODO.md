# TODO

  * Brenner 2025 alle Modelle + Kovarianzmatrizen
  * Latenzzeit, DDREF konstant pro Peson / Cancer Site? Ulanowski Chart 2020 Fig 7
      * Pro Organ+Person konstant, zwischen Organen verschieden
  * EAR-ERR Übertrag unabhängig von Additiv / Multiplikativ (s. ProZES, Verhältnis der Basisraten)
  * Shiny GUI: Need to open settings, otherwise params not defined
  * Vignette for Shiny GUI
  * gen_exposure() needs to accept sex=NULL / missing for pop
  * allow Excel input data
  * negative ELR possible, example from ?get_lear_indiv
  * gen_exposure(): when n = 1 and cancer_site = vector -> treat cancer_site as list
  * Make sure that cancer site names are harmonized across lists, order of list components for cancer site etc. is correct for Map()
  * Structure of cancer lists, risk model list: Incidence, mortality
  * YLL according to Ulanowski et al. 2019 RADS eq. 15 or UNSCEAR 2006 Annex A, Appendix B4
  * Take into account Ulanowski et al. 2019 appendix incidence vs. mortality
  * Speed up
  * More detailed uncertainty for parameters following ProZES / RadRAT, in particular DDREF, possibly dependent on dose, dose rate "acute" vs. "chronic"
  * Smooth rates instead of interpolate?
  * Validate using ProZES, RadRAT, LARisk
  * Convenience function cancer="all_solid", outcome="incidence", region="Germany", exposure="acute" to choose latency, ddref, wt_transfer
  * More risk models
  * Leukemia / lymphoma: C91-C95, No CLL, ATL -> CLL (C91.1, C91.4) / ATL (C91.5) should not be in baseline
  * Solid C00-C14, C15-C26, C30-C39, C40-C41, C43, C45-C49, C50, C51-C58, C60-C63, C64-C68, C69-C72, C73-C75, C76-C80
      * C44 should be in baseline, also for mortality
  * Use cancer in 5 continents data?
  * Consider EPA https://www.epa.gov/system/files/documents/2025-03/402-r-99-003_508-d.pdf - e.g., misclassification uncertainty + bias correction
