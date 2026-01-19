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
            "Input data",
            tabName="tab_baseline",
            icon=icon("file-upload", lib="font-awesome")
        ),
        menuItem(
            "Risk measures",
            tabName="tab_lear",
            icon=icon("table", lib="font-awesome")
        ),
        menuItem(
            "About",
            tabName="tab_about",
            icon=icon("lightbulb", lib="font-awesome")
        )
    )
)
