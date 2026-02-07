dashboardSidebar(
    id="sidebar",
    skin="light",
    fixed=TRUE,
    minified=TRUE,
    collapsed=FALSE,
    status="primary",
    sidebarMenu(
        flat = FALSE,
        compact = FALSE,
        childIndent = TRUE,
        menuItem(
            "Define exposure",
            tabName="tab_exposure",
            icon=icon("bolt-lightning", lib="font-awesome")
        ),
        menuItem(
            "Settings",
            tabName="tab_settings",
            icon=icon("sliders", lib="font-awesome")
        ),
        menuItem(
            "Risk estimate",
            tabName="tab_lear",
            icon=icon("chart-column", lib="font-awesome")
        ),
        menuItem(
            "About",
            tabName="tab_about",
            icon=icon("lightbulb", lib="font-awesome")
        )
    )
)
