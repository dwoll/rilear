fluidPage(
    fluidRow(
        box(title="Lifetime Excess Absolute Risk",
            width=12,
            actionButton("apply_settings", "Apply Settings"),
            DT::dataTableOutput("table_lear"))
    )
)
