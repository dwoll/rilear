fluidPage(
    fluidRow(
        box(# height = "600px",
            title = "About",
            width = 12,
            # status = NULL, 
            # closable = FALSE,
            # maximizable = FALSE, 
            collapsible = FALSE,
            p("The", a("rilear", href="https://github.com/dwoll/rilear/"),
              "package for", a("R", href="https://www.r-project.org/"),
              "provides functions for assessing the lifetime excess absolute risk from radiation exposure. Based on models for excess relative and excess absolute cancer risk for external low-LET radiation exposure. Risk measures include LAR / LEAR / CER, REID / REIC, ELR, and RADS. Supports multiple exposure events and simultaneous application to a whole population to project total number of expected cancer cases."),

            h6("Author"),
            p("'rilear' and this web application are written by:", br(),
              "Daniel", HTML("Wollschl&auml;ger"), br(),
              "Institute for Medical Biostatistics, Epidemiology und Informatics",
              a("(IMBEI)", href="https://www.unimedizin-mainz.de/imbei/"), br(),  
                "University Medical Center Mainz of the Johannes Gutenberg University Mainz", br(),  
                "Germany", br(),
              a("<wollschlaeger@uni-mainz.de>", href="mailto:wollschlaeger@uni-mainz.de"), br()),

            h6("Documentation"),
            p(a("Source code rilear",
                href="https://github.com/dwoll/rilear/"))
        ),
        
        box(title="References",
            width=12,
            collapsible = FALSE,
            tags$ul(tags$li("Sommer et al. Radiat Res 2025. DOI: 10.1667/RADE-24-00060.1"),
                    tags$li("ProZES: Ulanowski et al. Radiat Environ Biophys 2020. DOI: 10.1007/s00411-020-00866-7"),
                    tags$li("RadRAT: Berrington de Gonzalez et al. JRP 2012. DOI: 10.1088/0952-4746/32/3/205"),
                    tags$li("Sasaki et al. J Radiat Prot Res 2023 DOI: 10.14407/jrpr.2022.00213"),
                    tags$li("Walsh et al. Radiat Environ Biophys 2019. DOI: 10.1007/s00411-021-00910-0"),
                    tags$li("LARisk: Lee et al. DOI: 10.32614/CRAN.package.LARisk"))
        ),
        box(title="Software",
            width=12,
            collapsible = FALSE,
            p("This web application is built with R, shiny, and bs4Dash.
                    The rilear package uses functionality provided by the R packages
                    parallelly and dplyr:"),
            tags$ul(tags$li("Bengtsson H. (2026). parallelly: Enhancing the 'parallel' Package.", br(),
                      a("https://CRAN.R-project.org/package=parallelly",
                        href="https://CRAN.R-project.org/package=parallelly")),
                    tags$li("Granjon D. (2025). bs4Dash: A 'Bootstrap 4' Version of 'shinydashboard'.", br(),
                      a("https://CRAN.R-project.org/package=bs4Dash",
                        href="https://CRAN.R-project.org/package=bs4Dash")),
                    tags$li("Posit Software, PBC. (2026). shiny: Web Application Framework for R.", br(),
                      a("https://shiny.posit.co/",
                        href="https://shiny.posit.co/")),
                    tags$li("Wickham H, et al. (2026). dplyr: A Grammar of Data Manipulation.", br(),
                      a("https://CRAN.R-project.org/package=dplyr",
                        href="https://CRAN.R-project.org/package=dplyr")),
                    tags$li("Wollschlaeger D. (2026). rilear: Radiation Induced Lifetime Excess Absolute Risk.", br(),
                      a("https://github.com/dwoll/rilear/",
                        href="https://github.com/dwoll/rilear/"), "- development version"))
        )
    )
)
