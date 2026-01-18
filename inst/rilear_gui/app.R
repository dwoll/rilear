#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------
## rilear shiny App
## Daniel Wollschlaeger <wollschlaeger@uni-mainz.de>
#####---------------------------------------------------------------------------
#####---------------------------------------------------------------------------

library(shiny)
library(rilear)
library(bs4Dash)

source("global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## app code
#####---------------------------------------------------------------------------

shinyApp(
    #####-----------------------------------------------------------------------
    ## UI
    #####-----------------------------------------------------------------------
    ui=dashboardPage(
        # theme = "custom.css",
        title="Lifetime Excess Absolute Risk after Radiation Exposure",
        dark=NULL,
        help=FALSE,
        sidebar=source("app_ui_sidebar.R", encoding="UTF8")$value,
        header=dashboardHeader(
            tags$code(tags$h3("Lifetime Excess Absolute Risk after Radiation Exposure"))
        ),
        body=dashboardBody(
            tabItems(
                tabItem(
                    tabName="tab_baseline",
                    source("app_ui_tab_baseline.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_exposure",
                    source("app_ui_tab_exposure.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_lear",
                    source("app_ui_tab_lear.R", local=TRUE, encoding="UTF8")$value
                ),
                tabItem(
                    tabName="tab_about",
                    source("app_ui_tab_about.R", local=TRUE, encoding="UTF8")$value
                )
            )
        )
    ),
    #####-----------------------------------------------------------------------
    ## server
    #####-----------------------------------------------------------------------
    server=function(input, output, session) {
        session$onSessionEnded(stopApp)

        ## reactive conductor
        d_pop <- reactive({
            input$apply_data
            isolate({
                d_pop_sel <- if(input$d_pop_in == '1') {
                    d_pop_ger_country_2024L
                } else if(input$d_pop_in == '2') {
                    d_pop_ger_fedstate_2024L
                } else if(input$d_pop_in == '3') {
                    d_pop_ger_district_2024L
                } else if(input$d_pop_in == '4') {
                    argL <- if(!is.null(input$d_pop_upload)) {
                        list(x=input$d_pop_upload$datapath)
                    } else {
                        NULL
                    }
                    
                    do.call(read_d_pop, argL)
                } else {
                    NULL
                }

                return(d_pop_sel)
            })
        })

        ## reactive conductor
        d_mort <- reactive({
            input$apply_data
            isolate({
                d_mort_sel <- if(input$d_mort_in == '1') {
                    d_lifetable_ger_2024W
                } else if(input$d_mort_in == '2') {
                    d_mort_rate_ger_country_2024W_i
                } else if(input$d_mort_in == '3') {
                    d_mort_rate_ger_fedstate_2017W_i
                } else if(input$d_mort_in == '4') {
                    d_mort_rate_ger_district_2017W_i
                } else if(input$d_mort_in == '5') {
                    argL <- if(!is.null(input$d_mort_upload)) {
                        list(x=input$d_mort_upload$datapath)
                    } else {
                        NULL
                    }
                    
                    do.call(read_d_mort, argL)
                } else {
                    NULL
                }
                
                return(d_mort_sel)
            })
        })

        ## reactive conductor
        d_cancer <- reactive({
            input$apply_data
            isolate({
                d_cancer_sel <- if(input$d_cancer_in == '1') {
                    d_cancer_ger_incid_solidW_i
                } else if(input$d_cancer_in == '2') {
                    d_cancer_ger_incid_solid_c44W_i
                } else if(input$d_cancer_in == '3') {
                    d_cancer_ger_incid_breastW_i
                } else if(input$d_cancer_in == '4') {
                    d_cancer_ger_incid_leuk_lymphW_i
                } else if(input$d_cancer_in == '5') {
                    d_cancer_ger_mort_solidW_i
                } else if(input$d_cancer_in == '6') {
                    d_cancer_ger_mort_breastW_i
                } else if(input$d_cancer_in == '7') {
                    d_cancer_ger_mort_leuk_lymphW_i
                } else if(input$d_cancer_in == '8') {
                    argL <- if(!is.null(input$d_cancer_upload)) {
                        list(x=input$d_cancer_upload$datapath)
                    } else {
                        NULL
                    }
                    
                    do.call(read_d_cancer, argL)
                } else {
                    NULL
                }
                
                return(d_cancer_sel)
            })
        })
        
        ## reactive conductor
        l_risk_model <- reactive({
            input$apply_data
            isolate({
                l_risk_model_sel <- if(input$d_risk_model_in == '1') {
                    rm_solid_incid_walsh2021()
                } else if(input$d_risk_model_in == '2') {
                    rm_solid_incid_sumray()
                } else if(input$d_risk_model_in == '3') {
                    rm_breast_incid_walsh2021()
                } else if(input$d_risk_model_in == '4') {
                    rm_leuk_incid_walsh2021()
                } else if(input$d_risk_model_in == '5') {
                    rm_solid_mort_sumray()
                } else if(input$d_risk_model_in == '6') {
                    argL <- if(!is.null(input$d_risk_model_upload)) {
                        list(x=input$d_risk_model_upload$datapath)
                    } else {
                        NULL
                    }
                    
                    do.call(read_risk_model, argL)
                } else {
                    NULL
                }
                
                return(l_risk_model_sel)
            })
        })

        ## reactive conductor
        l_risk_model_mort <- reactive({
            input$apply_data
            isolate({
                l_risk_model_mort_sel <- if(input$d_risk_model_mort_in == '1') {
                    rm_solid_mort_sumray()
                } else if(input$d_risk_model_in == '2') {
                    argL <- if(!is.null(input$d_risk_model_mort_upload)) {
                        list(x=input$d_risk_model_mort_upload$datapath)
                    } else {
                        NULL
                    }
                    
                    do.call(read_risk_model_mort, argL)
                } else {
                    NULL
                }
                
                return(l_risk_model_sel)
            })
        })
        
        output$metrSelStruct <- renderUI({
            dvh <- DVH()$DVH
            if(!is.null(dvh)) {
                checkboxGroupInput("metrSelStruct",
                                   label=h5("Select structures"),
                                   choices=getStrIDs(dvh, what="structure", choices=TRUE),
                                   selected=1)
            } else {
                NULL
            }
        })

        observeEvent(
            eventExpr=input$metrSelStructAll,
            handlerExpr= {
                if(input$metrSelStructAll == 0) { return(NULL) }
                dvh <- DVH()$DVH
                if(!is.null(dvh)) {
                    # browser()
                    choices <- getStrIDs(dvh, what="structure", choices=TRUE)
                    if((input$metrSelStructAll %% 2) == 0) {
                        updateCheckboxGroupInput(session, "metrSelStruct", "Select structures",
                                                 choices=choices)
                    } else {
                        updateCheckboxGroupInput(session, "metrSelStruct", "Select structures",
                                                 choices=choices,
                                                 selected=choices)
                    }
                }
            }
        )

        output$metrSelPat <- renderUI({
            dvh <- DVH()$DVH
            if(!is.null(dvh)) {
                checkboxGroupInput("metrSelPat",
                                   label=h5("Select patients"),
                                   choices=getStrIDs(dvh, what="patient", choices=TRUE),
                                   selected=1)
            } else {
                NULL
            }
        })

        ## https://stackoverflow.com/a/35044158
        observeEvent(
            eventExpr=input$metrSelPatAll,
            handlerExpr= {
                if(input$metrSelPatAll == 0) { return(NULL) }
                dvh <- DVH()$DVH
                if(!is.null(dvh)) {
                    # browser()
                    choices <- getStrIDs(dvh, what="patient", choices=TRUE)
                    if((input$metrSelPatAll %% 2) == 0) {
                        updateCheckboxGroupInput(session, "metrSelPat", "Select patients",
                                                 choices=choices)
                    } else {
                        updateCheckboxGroupInput(session, "metrSelPat", "Select patients",
                                                 choices=choices,
                                                 selected=choices)
                    }
                }
            }
        )

        output$plotSelStruct <- renderUI({
            dvh <- DVH()$DVH
            if(!is.null(dvh)) {
                checkboxGroupInput("plotSelStruct",
                                   label=h5("Select structures"),
                                   choices=getStrIDs(dvh, what="structure", choices=TRUE),
                                   selected=1)
            } else {
                NULL
            }
        })

        observeEvent(
            eventExpr=input$plotSelStructAll,
            handlerExpr= {
                if(input$plotSelStructAll == 0) { return(NULL) }
                dvh <- DVH()$DVH
                if(!is.null(dvh)) {
                    # browser()
                    choices <- getStrIDs(dvh, what="structure", choices=TRUE)
                    if((input$plotSelStructAll %% 2) == 0) {
                        updateCheckboxGroupInput(session, "plotSelStruct", "Select structures",
                                                 choices=choices)
                    } else {
                        updateCheckboxGroupInput(session, "plotSelStruct", "Select structures",
                                                 choices=choices,
                                                 selected=choices)
                    }
                }
            }
        )

        output$plotSelPat <- renderUI({
            dvh <- DVH()$DVH
            if(!is.null(dvh)) {
                checkboxGroupInput("plotSelPat",
                                   label=h5("Select patients"),
                                   choices=getStrIDs(dvh, what="patient", choices=TRUE),
                                   selected=1)
            } else {
                NULL
            }
        })

        output$metrics <- DT::renderDataTable({
            dvh        <- DVH()$DVH
            sortOpts   <- c('1'="observed", '2'="structure", '3'="metric", '4'="patID")
            splitOpts  <- c('1'="structure", '2'="metric", '3'="patID")
            selMetrics <- if(length(input$metrInput) > 0) {
                metrRaw <- unlist(strsplit(input$metrInput, "[[:blank:],]"))
                metrRaw[nzchar(metrRaw)]
            } else {
                c("DMEAN", "D1CC", "V10%")
            }
            selStruct <- if(length(input$metrSelStruct) > 0) {
                if(!is.null(dvh)) {
                    getStrIDs(dvh, what="structure")[as.numeric(input$metrSelStruct)]
                } else {
                    NULL
                }
            } else {
                NULL
            }
            selPat <- if(length(input$metrSelPat) > 0) {
                if(!is.null(dvh)) {
                    getStrIDs(dvh, what="patient")[as.numeric(input$metrSelPat)]
                } else {
                    NULL
                }
            } else {
                NULL
            }
            interp  <- "linear" # c("linear", "spline", "ksmooth")[as.numeric(input$metrInterp)]
            EUDa    <- if(input$metrEUDa  != "") { as.numeric(input$metrEUDa)  } else { NULL }
            EUDfd   <- if(input$metrEUDfd != "") { as.numeric(input$metrEUDfd) } else { NULL }
            EUDab   <- if(input$metrEUDab != "") { as.numeric(input$metrEUDab) } else { NULL }

            NTCPtype    <- c("probit", "logit", "poisson", "relative_seriality")[as.numeric(input$metrNTCPtype)]
            NTCPtd50    <- if(input$metrNTCPtd50    != "") { as.numeric(input$metrNTCPtd50)    } else { NULL }
            NTCPn       <- if(input$metrNTCPn       != "") { as.numeric(input$metrNTCPn)       } else { NULL }
            NTCPm       <- if(input$metrNTCPm       != "") { as.numeric(input$metrNTCPm)       } else { NULL }
            NTCPgamma50 <- if(input$metrNTCPgamma50 != "") { as.numeric(input$metrNTCPgamma50) } else { NULL }
            NTCPs       <- if(input$metrNTCPs       != "") { as.numeric(input$metrNTCPs)       } else { NULL }
            
            sortSel <- input$metrSortBy
            sortBy <- if(length(sortSel) > 0) {
                sortOpts[sortSel]
            } else {
                NULL
            }
            if(!is.null(dvh)) {
                argL <- list(x=dvh,
                             metric=selMetrics,
                             patID=selPat,
                             structure=selStruct,
                             sortBy=sortBy,
                             interp=interp,
                             EUDa=EUDa, EUDfd=EUDfd, EUDab=EUDab,
                             NTCPtype=NTCPtype, NTCPtd50=NTCPtd50, NTCPn=NTCPn, NTCPm=NTCPm, NTCPgamma50=NTCPgamma50, NTCPs=NTCPs,
                             TCPtype=NTCPtype,  TCPtcd50=NTCPtd50,  TCPn=NTCPn,  TCPm=NTCPm,  TCPgamma50=NTCPgamma50,  TCPs=NTCPs)
                argL <- Filter(Negate(is.null), argL)
                metr <- do.call(getMetric, argL)
                metr$observed <- round(metr$observed, 2)
                metr
            } else {
                NULL
            }
        })#, options=list(pageLength=25))
    }
)
