# TODO

  * Adjust help pages for changed functions
  * Speed up
  * Automatically calculate risks for several / all available cancer sites / risk models
  * Simulate DDREF, possibly dependent on dose, dose rate "acute" vs. "chronic"
  * REIC: calc excess cancer mortality for Surv_E even for REIC?
  * Smooth rates instead of interpolate?
  * Validate using ProZES, RadRAT, LARisk
  * Convenience function cancer="all_solid", outcome="incidence", region="Germany", exposure="acute"
  * More detailed uncertainty for parameters following ProZES / RadRAT
  * Leukemia / lymphoma mortality model
  * Breast cancer mortality model
  * Leukemia / lymphoma: C91-C95, No CLL, ATL -> CLL (C91.1, C91.4) / ATL (C91.5) not in in baseline -> What now?
  * Solid C00-C14, C15-C26, C30-C39, C40-C41, C43, C45-C49, C50, C51-C58, C60-C63, C64-C68, C69-C72, C73-C75, C76-C80
  * Use cancer in 5 continents data?
  * Shiny app with map for region selection
  * Consider EPA https://www.epa.gov/system/files/documents/2025-03/402-r-99-003_508-d.pdf - e.g., misclassification uncertainty + bias correction