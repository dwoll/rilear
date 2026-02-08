dashboardControlbar(
    id="controlbar",
    skin="light",
    controlbar_collapsed=TRUE,
    column(
        width = 12,
        align = "left",
        radioButtons(
            "cntrl_month_week",
            "Group by date",
            c("Month"="month",
              "Week"="week")
        ),
        radioButtons(
            "cntrl_dsr_standard",
            "Standard population:",
            c("Germany 2011"="Germany_2011",
              #"Deutschland 1987"="Germany_1987",
              "Europe 2013"="Europe_2013_90",
              "Europe WHO 1993"="Europe_WHO_1993",
              "Europe 1976"="Europe_1976"#,
              #"World"="World",
              #"OECD"="OECD"
              )
        )
    )
)
