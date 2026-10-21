run_gui <-
function(...) {
    appDir <- system.file("rilear_gui", package="rilear")
    if(!nzchar(appDir)) {
        stop("Could not find Shiny directory. Try re-installing 'rilear'.", call.=FALSE)
    }

    pkg_need <- c("shiny", "bs4Dash", "leaflet", "DT", "terra", "shinyWidgets")
    pkg_try  <- vapply(pkg_need, requireNamespace, quietly=TRUE,
                       FUN.VALUE=logical(1))

    pkg_have <- pkg_need[ pkg_try]
    pkg_miss <- pkg_need[!pkg_try]
    if(length(pkg_miss) > 0L) {
        stop(paste0("Missing package(s) '",
                    paste(pkg_miss, collapse="', '"),
                    "' required for the Shiny GUI"),
             call.=FALSE)
    }

    bs4Dash_version <- packageVersion("bs4Dash")
    if(compareVersion("2.0.0", as.character(bs4Dash_version)) <= 0) {
        shiny::runApp(appDir, ...)
    } else {
        stop("Need bs4Dash version 2.0.0 or above.")
    }
}
