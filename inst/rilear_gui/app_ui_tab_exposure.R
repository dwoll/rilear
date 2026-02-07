fluidPage(
    fluidRow(
        box(title="Define exposure events",
            width=12,
            radioButtons("exposure_target",
                         label=h4("Select exposure target"),
                         list("Individual person"="indiv",
                              "Population"       ="pop"),
                         inline=TRUE),
            uiOutput("ui_target_indiv"),
            uiOutput("ui_target_pop"),
            uiOutput("ui_target_pop_map"),
            # uiOutput("ui_target_pop_ags"),
            uiOutput("ui_n_expo_events"),
            uiOutput("ui_expo_events")
        )
    )
)
