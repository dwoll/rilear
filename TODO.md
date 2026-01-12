## TODO

  * Automatically calculate risks for several / all available cancer sites / risk models
  * Default "fixed" for dose distribution, default ddref=1
  * Separate DDREF for each exposure event
  * Consider dose and dose rate to select DDREF
  * REIC: calc excess cancer mortality for Surv_E even for REIC?
  * Smooth rates instead of interpolate?
  * Validate using ProZES, RadRAT, LARisk
  * Convenience function cancer="all_solid", outcome="incidence", region="Germany", "dose_rate=acute"
  * More detailed uncertainty for parameters following ProZES / RadRAT
  * Leukemia / lymphoma mortality model
  * Breast cancer mortality model
  * Leukemia / lymphoma: C91-C95, No CLL, ATL -> CLL (C91.1, C91.4) / ATL (C91.5) not in in baseline -> What now?
  * Solid C00-C14, C15-C26, C30-C39, C40-C41, C43, C45-C49, C50, C51-C58, C60-C63, C64-C68, C69-C72, C73-C75, C76-C80
  * Shiny app

  \item{cancer}{If \code{lat_t0} and \code{lat_eta} are not provided, used for defining the parameter default values for the latency function. Solid cancer: \code{lat_t0} = 4 and \code{lat_eta} = 6.25 for \code{lat_method} = \code{"ProZES"}; \code{lat_t0} = 7.5 for \code{lat_method} = \code{"RadRAT"}. Leukemia / lymphoma: \code{lat_t0} = 1.5 and \code{lat_eta} = 7.66 for \code{lat_method} = \code{"ProZES"}; \code{lat_t0} = 2.25 for \code{lat_method} = \code{"RadRAT"}.}
