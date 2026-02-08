library(shiny)
library(rilear)
library(bs4Dash)
library(leaflet)
library(shinyWidgets)

source("global.R", encoding="UTF8")

#####---------------------------------------------------------------------------
## UI
#####---------------------------------------------------------------------------

ui <- dashboardPage(
    # theme = "custom.css",
    title="Lifetime Excess Absolute Risk after Radiation Exposure",
    dark=NULL,
    help=NULL,
    sidebar=source("app_ui_sidebar.R", encoding="UTF8")$value,
    controlbar=source("app_ui_controlbar.R", encoding="UTF8")$value,
    header=dashboardHeader(
        tags$code(tags$h3("Lifetime Excess Absolute Risk after Radiation Exposure"))
    ),
    body=dashboardBody(
        tabItems(
            tabItem(
                tabName="tab_exposure",
                source("app_ui_tab_exposure.R", local=TRUE, encoding="UTF8")$value
            ),
            tabItem(
                tabName="tab_settings",
                source("app_ui_tab_settings.R", local=TRUE, encoding="UTF8")$value
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
)

#####---------------------------------------------------------------------------
## server
#####---------------------------------------------------------------------------

server <- function(input, output, session) {
    output$map_pop_in <- renderLeaflet({
        if(!is.null(input$d_pop_in) &&
           !(input$d_pop_in %in% c("ger_country", "custom"))) {
            map_sel <- l_map[[input$d_pop_in]]
                
            leaflet() |>
                addTiles() |>
                setView(lng=10.5, lat=51.3, zoom=6) |>
                addPolygons(data       =map_sel,
                            fillColor  ="white",
                            fillOpacity=0.5,
                            color      ="black",
                            stroke     =TRUE,
                            weight     =1,
                            ## need different ID than other layer
                            ## must be unique across whole map
                            layerId    =~OBJID,
                            group      ="regions",
                            label      =~GEN) |>
                addPolygons(data       =map_sel,
                            fillColor  ="red",
                            fillOpacity=0.5,
                            weight     =1,
                            color      ="black",
                            stroke     =TRUE,
                            layerId    =~DEBKG_ID,
                            group      =~AGS,
                            label      =~GEN) |>
                hideGroup(group=map_sel[["AGS"]])
        } else {
            NULL
        }
    })
    
    output$ui_target_indiv <- renderUI({
        if(input$exposure_target == "indiv") {
            radioButtons("expo_sex",
                         label="Sex",
                         list("Female"="f",
                              "Male"  ="m"),
                         selected="f",
                         inline=TRUE)
        } else {
            NULL
        }
    })

    #define leaflet proxy for updating etc.
    map_pop_in_proxy <- leafletProxy("map_pop_in")
    map_sel_regions  <- reactiveValues()

    # map_pop_in_update <- function(pop_in) {
    #     map_sel <- l_map[[pop_in]]
    #     map_pop_in_proxy |>
    #         clearShapes() |>
    #         clearControls() |>
    #         clearMarkers() |>
    #         addTiles() |>
    #         setView(lng=10.5, lat=51.3, zoom=6) |>
    #         addPolygons(data       =map_sel,
    #                     fillColor  ="white",
    #                     fillOpacity=0.5,
    #                     color      ="black",
    #                     stroke     =TRUE,
    #                     weight     =1,
    #                     ## need different ID than other layer
    #                     ## must be unique across whole map
    #                     layerId    =~OBJID,
    #                     group      ="regions",
    #                     label      =~GEN) |>
    #         addPolygons(data       =map_sel,
    #                     fillColor  ="red",
    #                     fillOpacity=0.5,
    #                     weight     =1,
    #                     color      ="black",
    #                     stroke     =TRUE,
    #                     layerId    =~DEBKG_ID,
    #                     group      =~AGS,
    #                     label      =~GEN) |>
    #         hideGroup(group=map_sel[["AGS"]])
    # }
    
    # observe({
    #     pop_in <- input$d_pop_in
    #     if(!is.null(pop_in)) {
    #         if(!(pop_in %in% c("ger_country", "custom"))) {
    #             map_pop_in_update(pop_in)
    #         }
    #     }
    # })
    
    ## when region is clicked on map
    ## update map_sel_regions reactive values
    ## add / remove from selectizeInput() widget
    # observeEvent(input$map_pop_in_click, {
    #     tmp <- 0
    # })
    
    observeEvent(input$map_pop_in_shape_click, {
        if(input$map_pop_in_shape_click$group == "regions") {
            d_click_id <- ld_region[[input$d_pop_in]] |>
                dplyr::filter(OBJID %in% input$map_pop_in_shape_click$id)

            map_sel_regions$groups <- c(map_sel_regions$groups,
                                        unique(d_click_id[["AGS"]])) |>
                unique()
            
            map_pop_in_proxy |>
                showGroup(group=unique(d_click_id[["AGS"]]))
        } else {
            d_click_id <- ld_region[[input$d_pop_in]] |>
                dplyr::filter(DEBKG_ID %in% input$map_pop_in_shape_click$id)
            
            map_sel_regions$groups <- setdiff(map_sel_regions$groups,
                                              unique(d_click_id[["AGS"]]))
            
            map_pop_in_proxy |>
                hideGroup(group=input$map_pop_in_shape_click$group)
        }
        
        d_sel_id <- ld_region[[input$d_pop_in]] |>
            dplyr::filter(AGS %in% map_sel_regions$groups)
        
        # updateSelectizeInput(session,
        #                      inputId ="sel_pop_in_ags",
        #                      choices =l_region_inv[[input$d_pop_in]],
        #                      selected=unique(d_sel_id[["AGS"]]))
        updatePickerInput(session,
                          inputId ="sel_pop_in_ags",
                          choices =l_region_inv[[input$d_pop_in]],
                          selected=unique(d_sel_id[["AGS"]]))
    })
    
    ## if regions are added / removed via the selectizeInput()
    ## update map_sel_regions reactive values
    ## widget directly, add / remove them from the map, as well
    observeEvent(input$sel_pop_in_ags, {
        removed_via_selectInput <- setdiff(map_sel_regions$groups,
                                           input$sel_pop_in_ags)
        
        added_via_selectInput <- setdiff(input$sel_pop_in_ags,
                                         map_sel_regions$groups)
        
        ## update reactive values and map
        if(length(removed_via_selectInput) > 0) {
            map_sel_regions$groups <- input$sel_pop_in_ags
            map_pop_in_proxy |>
                hideGroup(group=removed_via_selectInput)
        }
        
        if(length(added_via_selectInput) > 0) {
            map_sel_regions$groups <- input$sel_pop_in_ags
            map_pop_in_proxy |>
                showGroup(group=added_via_selectInput)
        }
    }, ignoreNULL=FALSE)
    
    output$ui_target_pop <- renderUI({
        if(input$exposure_target == "pop") {
            tagList(selectInput("d_pop_in",
                                label="Select population data",
                                list("Germany"                 ="ger_country",
                                     "Germany - federal states"="ger_fedstate",
                                     "Germany - districts"     ="ger_district",
                                     "Upload custom data"      ="custom")),
                    conditionalPanel(condition="input.d_pop_in == 'custom'",
                                     h5("Upload population file: "),
                                     radioButtons("d_pop_type", "File format:",
                                                  list("Plain text"=1, "R"=2)),
                                     fileInput("d_pop_upload", "Select file:", multiple=FALSE)))
        } else {
            NULL
        }
    })

    output$ui_target_pop_map <- renderUI({
        if((input$exposure_target == "pop") &&
           !is.null(input$d_pop_in)         &&
           !(input$d_pop_in %in% c("ger_country", "custom"))) {
            # map_sel_regions$groups <- vector()
            region_inv <- l_region_inv[[input$d_pop_in]]
            tagList(leafletOutput("map_pop_in"),
                    # selectizeInput("sel_pop_in_ags",
                    #                label="Selected regions",
                    #                choices=region_inv,
                    #                selected=region_inv,
                    #                multiple=TRUE),
                    pickerInput(
                        inputId ="sel_pop_in_ags", 
                        label   ="Selected regions",
                        choices =region_inv, 
                        selected=region_inv,
                        multiple=TRUE,
                        options =pickerOptions(
                            actionsBox=TRUE, 
                            size      =10,
                            selectedTextFormat="count > 5"
                        )))
        }
    })
    
    output$ui_target_pop_ags <- renderUI({
        if((input$exposure_target == "pop") &&
           !is.null(input$d_pop_in)         &&
           !(input$d_pop_in %in% c("ger_country", "custom"))) {
            region <- l_region[[input$d_pop_in]]
            tagList(selectizeInput("sel_pop_in_ags",
                                   label="Selected regions",
                                   choices=region,
                                   selected=NULL,
                                   multiple=TRUE))
        } else {
            NULL
        }
    })
    
    output$ui_n_expo_events <- renderUI({
        numericInput("expo_n_events",
                     label="Number of exposure events (1 / year)",
                     value=1L,
                     min=1L,
                     max=10L,
                     step=1L)
    })
    
    output$ui_expo_events <- renderUI({
        n_expo_events <- input$expo_n_events
        l_out0 <- if(!is.null(n_expo_events)) {
            lapply(seq_len(n_expo_events), function(expo_event_i) {
                ui_dr <- radioButtons(sprintf("expo_dose_rate_%.2d", expo_event_i),
                                      label="Dose rate",
                                      list("Acute"  ="acute",
                                           "Chronic"="chronic"),
                                      inline=TRUE)
                
                ui_ddref <- numericInput(sprintf("expo_dose_ddref_%.2d", expo_event_i),
                                         label="DDREF",
                                         value=1,
                                         min=0.1,
                                         max=10,
                                         step=0.1)
                
                ui_dd <- selectInput(sprintf("expo_dose_distr_%.2d", expo_event_i),
                                     label="Dose distribution",
                                     as.list(rilear:::dose_distr_have))
                
                ui_dp1 <- conditionalPanel(condition=sprintf("input.expo_dose_distr_%.2d == 'fixed'", expo_event_i),
                                           #h5("Dose distribution parameter: "),
                                           numericInput(sprintf("expo_dose_param_fix1_%.2d", expo_event_i),
                                                        label="Dose value (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01))
                
                ui_dp2 <- conditionalPanel(condition=sprintf("input.expo_dose_distr_%.2d == 'normal'", expo_event_i),
                                           #h5("Dose distribution parameter: "),
                                           numericInput(sprintf("expo_dose_param_norm1_%.2d", expo_event_i),
                                                        label="Dose mean (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=4,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_norm2_%.2d", expo_event_i),
                                                        label="Dose SD (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01))
                
                ui_dp3 <- conditionalPanel(condition=sprintf("input.expo_dose_distr_%.2d == 'lognormal'", expo_event_i),
                                           #h5("Dose distribution parameter: "),
                                           numericInput(sprintf("expo_dose_param_lnorm1_%.2d", expo_event_i),
                                                        label="Dose geometric mean (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_lnorm2_%.2d", expo_event_i),
                                                        label="Dose geometric SD (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01))
                
                ui_dp4 <- conditionalPanel(condition=sprintf("input.expo_dose_distr_%.2d == 'triangular'", expo_event_i),
                                           #h5("Dose distribution parameter: "),
                                           numericInput(sprintf("expo_dose_param_tri1_%.2d", expo_event_i),
                                                        label="Dose mode (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_tri2_%.2d", expo_event_i),
                                                        label="Dose minimum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_tri3_%.2d", expo_event_i),
                                                        label="Dose maximum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01))
                
                ui_dp5 <- conditionalPanel(condition=sprintf("input.expo_dose_distr_%.2d == 'logtriangular'", expo_event_i),
                                           #h5("Dose distribution parameter: "),
                                           numericInput(sprintf("expo_dose_param_ltri1_%.2d", expo_event_i),
                                                        label="Dose log-mode (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_ltri2_%.2d", expo_event_i),
                                                        label="Dose log-minimum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_ltri3_%.2d", expo_event_i),
                                                        label="Dose log-maximum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01))
                
                ui_dp6 <- conditionalPanel(condition=sprintf("input.expo_dose_distr_%.2d == 'uniform'", expo_event_i),
                                           #h5("Dose distribution parameter: "),
                                           numericInput(sprintf("expo_dose_param_unif1_%.2d", expo_event_i),
                                                        label="Dose minimum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_unif2_%.2d", expo_event_i),
                                                        label="Dose maximum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01))
                
                ui_dp7 <- conditionalPanel(condition=sprintf("input.expo_dose_distr_%.2d == 'loguniform'", expo_event_i),
                                           #h5("Dose distribution parameter: "),
                                           numericInput(sprintf("expo_dose_param_lunif1_%.2d", expo_event_i),
                                                        label="Dose log-minimum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01),
                                           numericInput(sprintf("expo_dose_param_lunif2_%.2d", expo_event_i),
                                                        label="Dose log-maximum (Sv)",
                                                        value=NA_real_,
                                                        min=0,
                                                        max=3,
                                                        step=0.01))
                
                ui_agex_timing <- if(input$exposure_target == "indiv") {
                    numericInput(sprintf("expo_agex_timing_%.2d", expo_event_i),
                                         label="Age at exposure",
                                         value=20L,
                                         min=0L,
                                         max=99L,
                                         step=1L)
                } else if(input$exposure_target == "pop") {
                    numericInput(sprintf("expo_agex_timing_%.2d", expo_event_i),
                                 label="Year of exposure - relative to age",
                                 value=0L,
                                 min=0L,
                                 max=99L,
                                 step=1L)
                } else {
                    NULL
                }

                l_out_i <- tagList(ui_agex_timing, ui_dr, ui_ddref, ui_dd,
                                   ui_dp1, ui_dp2,
                                   ui_dp3, ui_dp4,
                                   ui_dp5, ui_dp6, ui_dp7)
                
                tagList(h4(sprintf("Exposure event %d", expo_event_i)),
                        l_out_i)
            })
        } else {
            NULL
        }


        l_out <- Filter(Negate(is.null), l_out0)
        l_out
    })

    output$ui_settings_pop_specific <- renderUI({
        if(isTRUE(input$exposure_target == "pop")) {
            tagList(checkboxInput("settings_pop_stratify_sex",
                                  "Stratify results by sex?",
                                  value=TRUE),
                    numericInput("settings_pop_pop_ref",
                                 label="Reporting risk: Per how many persons?",
                                 value=10000L,
                                 min=1L,
                                 max=1000000L,
                                 step=5000L))
        } else {
            NULL
        }
    })
    
    output$ui_settings_multicore <- renderUI({
        if(isTRUE(input$settings_multicore)) {
            tagList(numericInput("settings_n_cores_max",
                                 label="Max number of cores to use",
                                 value=1L,
                                 min=1L,
                                 max=99L,
                                 step=1L),
                    numericInput("settings_n_cores_omit",
                                 label="Spare at least how many cores?",
                                 value=2L,
                                 min=1L,
                                 max=99L,
                                 step=1L),
            )
        } else {
            NULL
        }
    })
    
    get_d_pop_in <- reactive({
        if(!is.null(input$d_pop_in)) {
            if(input$d_pop_in == "ger_country") {
                d_pop_ger_country_2024L
            } else if(input$d_pop_in      == "ger_fedstate") {
                ags_sel <- input$sel_pop_in_ags
                if(!is.null(ags_sel)) {
                    d_pop_ger_fedstate_2024L |>
                        dplyr::filter(ags %in% ags_sel)
                } else {
                    d_pop_ger_fedstate_2024L
                }
            } else if(input$d_pop_in      == "ger_district") {
                ags_sel <- input$sel_pop_in_ags
                if(!is.null(ags_sel)) {
                    d_pop_ger_district_2024L |>
                        dplyr::filter(ags %in% ags_sel)
                } else {
                    d_pop_ger_district_2024L
                }
            } else {
                ## TODO custom
                NULL
            }
        } else {
            NULL
        }
    })
    
    get_expo_event <- reactive({
        get_all <- function(nn, elem, type, len) {
            vapply(seq_len(nn), function(i) {
                input[[sprintf(paste0(elem, "_%.2d"), i)]]
            }, type(len))
        }
        
        get_all_dp <- function(nn, elem, type, len) {
            lapply(seq_len(nn), function(i) {
                p1 <- input[[sprintf(paste0("expo_dose_param_", elem, "1_%.2d"), i)]]
                p2 <- input[[sprintf(paste0("expo_dose_param_", elem, "2_%.2d"), i)]]
                p3 <- input[[sprintf(paste0("expo_dose_param_", elem, "3_%.2d"), i)]]
                c(p1, p2, p3)[seq_len(len)]
            })
        }
        
        n <- input$expo_n_events
        if(!is.null(n)) {
            dose_rate        <- get_all(   n, "expo_dose_rate",   character, 1L)
            dose_ddref       <- get_all(   n, "expo_dose_ddref",  numeric,   1L)
            dose_distr       <- get_all(   n, "expo_dose_distr",  character, 1L)
            dose_param_fix   <- get_all_dp(n, "fix",              numeric,   1L)
            dose_param_norm  <- get_all_dp(n, "norm",             numeric,   2L)
            dose_param_lnorm <- get_all_dp(n, "lnorm",            numeric,   2L)
            dose_param_tri   <- get_all_dp(n, "tri",              numeric,   3L)
            dose_param_ltri  <- get_all_dp(n, "ltri",             numeric,   3L)
            dose_param_unif  <- get_all_dp(n, "unif",             numeric,   2L)
            dose_param_lunif <- get_all_dp(n, "lunif",            numeric,   2L)
            agex_timing      <- get_all(n,    "expo_agex_timing", numeric, 1L)
            cancer_site      <- list(c("all_solid", "leuk_lymph"))
            
            l_dose_param <- list(fixed        =dose_param_fix,
                                 normal       =dose_param_norm,
                                 lognormal    =dose_param_lnorm,
                                 triangular   =dose_param_tri,
                                 logtriangular=dose_param_ltri,
                                 uniform      =dose_param_unif,
                                 loguniform   =dose_param_lunif)
            
            ddrs <- vapply(seq_len(n), function(ei) {
                input[[sprintf("expo_dose_distr_%.2d", ei)]] },
                FUN.VALUE=character(1))
            
            dose_param <- lapply(seq_along(ddrs), function(dd) {
                l_dose_param[[ddrs[dd]]][[dd]] })
            
            if(input$exposure_target == "indiv") {
                sex <- input[["expo_sex"]]
                gen_exposure(n          =n,
                             sex        =sex,
                             agex       =agex_timing,
                             dose_distr =dose_distr,
                             dose_param =dose_param,
                             ddref      =dose_ddref,
                             dose_rate  =dose_rate,
                             cancer_site=cancer_site)
            } else if(input$exposure_target == "pop") {
                sex <- "f" # does not matter for pop at this point
                gen_exposure(n          =n,
                             sex        =sex,
                             timing     =agex_timing,
                             dose_distr =dose_distr,
                             dose_param =dose_param,
                             ddref      =dose_ddref,
                             dose_rate  =dose_rate,
                             cancer_site=cancer_site)
            } else {
                NULL
            }
        } else {
            NULL
        } 
    })
    
    output$table_lear <- DT::renderDataTable({
        input$apply_settings
        
        d_lear       <- NULL
        cols_numeric <- integer(0)
        
        isolate({
            if(!is.null(input$exposure_target)) {
                rm      <- list(breast    =rm_breast_incid_walsh2021(),
                                all_solid =rm_solid_incid_walsh2021(),
                                leuk_lymph=rm_leuk_incid_walsh2021())
                
                rm_mort <- list(all_solid =rm_solid_mort_sumray(),
                                breast    =rm_solid_incid_sumray(),
                                leuk_lymph=rm_solid_mort_sumray())
                
                base_cancer <- list(all_solid =d_cancer_ger_incid_solidW_i,
                                    breast    =d_cancer_ger_incid_breastW_i,
                                    leuk_lymph=d_cancer_ger_incid_leuk_lymphW_i)
                
                base_cancer_mort <- list(all_solid =d_cancer_ger_mort_solidW_i,
                                         breast    =d_cancer_ger_mort_breastW_i,
                                         leuk_lymph=d_cancer_ger_mort_leuk_lymphW_i)
                
                wt_transfer <- list(all_solid =c(ERR=0.5, EAR=0.5),
                                    breast    =c(ERR=0.0, EAR=1.0),
                                    leuk_lymph=c(ERR=1.0, EAR=0.0))
                
                lat_t0 <- list(all_solid =5,
                               breast    =5,
                               leuk_lymph=1.5)
                
                lat_eta <- list(all_solid =6,
                                breast    =6,
                                leuk_lymph=6.75)
                
                expo_event        <- get_expo_event()
                n_sim             <- input$settings_n_sim
                lat_method        <- input$settings_lat_method
                lat_fixed         <- isTRUE(input$settings_lat_fixed)
                wt_transfer_fixed <- isTRUE(input$settings_wt_transfer_fixed)
                ddref_fixed       <- isTRUE(input$settings_ddref_fixed)
                multicore         <- isTRUE(input$settings_multicore)
                alpha             <- input$settings_alpha
                n_cores_max       <- input$settings_n_cores_max
                n_cores_omit      <- input$settings_n_cores_omit
                metric            <- input$settings_metric
                age_max           <- input$settings_age_max
                
                if(!is.null(expo_event)) {
                    d_lear <- if(input$exposure_target == "indiv") {
                        get_lear_indiv_mc(exposure         =expo_event,
                                          n_sim            =n_sim,
                                          wt_transfer      =wt_transfer,
                                          lat_t0           =lat_t0,
                                          lat_eta          =lat_eta,
                                          lat_method       =lat_method,
                                          lat_fixed        =lat_fixed,
                                          wt_transfer_fixed=wt_transfer_fixed,
                                          ddref_fixed      =ddref_fixed,
                                          risk_model       =rm,
                                          risk_model_mort  =rm_mort,
                                          alpha            =alpha,
                                          multicore        =multicore,
                                          n_cores_max      =n_cores_max,
                                          n_cores_omit     =n_cores_omit,
                                          aggr_mc          =TRUE,
                                          base_cancer      =base_cancer,
                                          base_cancer_mort =base_cancer_mort,
                                          d_base_mort      =d_lifetable_ger_2024W,
                                          metric           =metric,
                                          age_max          =age_max)
                        
                    } else if(input$exposure_target == "pop") {
                        stratify_sex <- isTRUE(input$settings_pop_stratify_sex)
                        pop_ref      <- input$settings_pop_pop_ref
                        d_pop        <- get_d_pop_in()
                        get_lear_pop(x                =d_pop,
                                     n_sim            =n_sim,
                                     exposure         =expo_event,
                                     wt_transfer      =wt_transfer,
                                     lat_t0           =lat_t0,
                                     lat_eta          =lat_eta,
                                     lat_method       =lat_method,
                                     lat_fixed        =lat_fixed,
                                     wt_transfer_fixed=wt_transfer_fixed,
                                     ddref_fixed      =ddref_fixed,
                                     risk_model       =rm,
                                     risk_model_mort  =rm_mort,
                                     stratify_sex     =stratify_sex,
                                     pop_ref          =pop_ref,
                                     alpha            =alpha,
                                     multicore        =multicore,
                                     n_cores_max      =n_cores_max,
                                     n_cores_omit     =n_cores_omit,
                                     base_cancer      =base_cancer,
                                     # base_cancer_mort =base_cancer_mort,
                                     d_base_mort      =d_lifetable_ger_2024W,
                                     metric           =metric,
                                     age_max          =age_max)                        
                    }
                    
                    cols_numeric <- unname(which(vapply(d_lear, is.numeric, logical(1))))
                }
            }
        })

        DT::datatable(d_lear,
                      extensions=c('Buttons', 'Scroller'),
                      options   =list(deferRender=TRUE,
                                      scrollX    =200,
                                      scroller   =TRUE,
                                      dom        ='Bfrtip',
                                      buttons    =list(list(extend ='collection',
                                                            buttons=c('csv', 'excel'),
                                                            text   ='Download')))) |>
            DT::formatRound(columns=cols_numeric, digits=2)
    })
}

shinyApp(ui, server)
