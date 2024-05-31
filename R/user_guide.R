# This page is a hidden page for the User Guide
tutorialPanel <- function() {
  tabPanel(
    value = "user_guide",
    "User Guide",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Dashboard User Guide"),
          h5("How to use this dashboard:", style = "font-family: GDS Transport, arial, sans-serif; font-size :20px; font-weight: bold"),
          tags$ul(
            tags$li("Use the navigation bar on the left to select which outcome/enabler/indicator page you want to view."),
            tags$li("Within each outcome/enabler page, select a geographical level (National, Regional, Local Authority) using the dropdown list at the top."),
            tags$li("When selecting Regional or Local Authority, you will need to use the adjacent dropdowns and use the checkboxes to further filter the data. You can also, type the name directly into the box."),
            tags$li("Your selected geographical level will be shown above the first set of indicators for visibility."),
            tags$li("Most figures, charts and tables within each outcome/enabler page will update based on your geographic level selection except for Regional Charts which won’t update. You need to select a new geographic level and new region/local authority for each outcome/enabler on the dashboard."),
            tags$li("Timeseries charts will update with extra lines when ‘Compare with National’ and/or ‘Compare with Regional’ are selected."),
            tags$li("The domain (theme) options for each outcome/enabler page will be displayed below the drop downs. Select a different domain by clicking on the text. Please note some outcome/enabler pages will only have one domain."),
            tags$li("All charts can be viewed as a table to view more detailed data at selected geographic levels, select ‘View chart as at table’ to view the table data for your geographic selection."),
            tags$li("Symbols used in data tables indicate the following:"),
            tags$ul(
              tags$li("c - confidential data which has been suppressed."),
              tags$li("k - rounds to 0 but is not 0."),
              tags$li("u – observation is of low reliability."),
              tags$li("x - data is unavailable for other reason(s)."),
              tags$li("z - observation is not applicable.")
            ),
            tags$li("Select ‘Additional information’ underneath charts and tables where available, to view footnotes regarding the data source and limitations for context."),
            tags$li("If you have selected ‘Local Authority’ as the geographic level you can also select to view a chart called ‘10 statistical neighbours’.  For more information on statistical neighbours, see…..<insert links or publication here>")
          )
        )
      ),
      gov_row(
        column(
          width = 12,
          tags$h5("Interactive Plots User Guide", style = "font-family: GDS Transport, arial, sans-serif; font-size :20px; font-weight: bold"),
          p("The menu bar along the top of the charts contains extra interactive features to:"),
          tags$ul(
            tags$li("Download chart as png file"),
            tags$li("Hover over lines/bars in the plot to see specific values."),
            tags$li("Zoom in and out"),
            tags$li("Auto scale to provide a clearer view on trends"),
            tags$li("Reset Axes back to the starting view"),
            tags$li("Compare data for each data point being hovered"),
            tags$h6("Using the Key", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px; font-weight: bold"),
            tags$li("Double clicking a line/value in the key will isolate the value in the plot."),
            tags$li("Double clicking the same value again will restore the original plot"),
            tags$li("Single clicking a line/value in the key will remove that line/value from the plot")
          )
        )
      ),
      # gov_row(
      #   column(
      #     width = 12,
      #     h1("Dashboard User Guide - EDIT THIS PAGE"),
      #     h5("How to use this dashboard:"),
      #     "Use the navigation bar on the left to select which tab you want to view.",
      #     tags$br(),
      #     tags$h5("Dashboard Structure", style = "font-family: GDS Transport, arial, sans-serif; font-size :20px; font-weight: bold"),
      #     tags$ul(
      #       tags$li(tags$b("Introduction - "), "a basic introduction to the tool and provides links to the research report and the technical report."),
      #       tags$li(tags$b("Earnings trajectory - "), "looks at the average earnings of individuals in employment. You can build on the
      #     presented plots by selecting breakdowns you wish to see and compare. There is also a function to compare with the overall average for all individuals."),
      #       tags$li(tags$b("Main activities - "), "looks at the main activities for individuals. You can compare the main activities by selecting multiple breakdowns."),
      #       tags$li(tags$b("Accessibility statement - "), "contains the accessibility statement for this tool."),
      #       tags$li(tags$b("Feedback and suggestions - "), "contains links for a user feedback form and a form for reporting any bugs or errors found within the tool.")
      #     ),
      #     tags$br(),
      #     tags$h5("Interactive Plots User Guide", style = "font-family: GDS Transport, arial, sans-serif; font-size :20px; font-weight: bold"),
      #     p("")
      #     tags$ul(
      #       tags$li("Hover over lines/bars in the plot to see specific values."),
      #       tags$li("The bar along the top of the plots contains extra interactive features such as download as PNG and/or resize plot and zoom."),
      #       # tags$br(),
      #       tags$h6("Using the Key", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px; font-weight: bold"),
      #       tags$li("Double clicking a line/value in the key will isolate the value in the plot."),
      #       tags$li("Double clicking the same value again will restore the original plot"),
      #       tags$li("Single clicking a line/value in the key will remove that line/value from the plot")
      #     )
      #   )
      # )
    )
  )
}
