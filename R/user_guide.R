# This page is a hidden page for the User Guide
tutorialPanel <- function() {
  tabPanel(
    value = "user_guide",
    "User guide",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1("Dashboard user guide"),
          tags$br(),
          h2("How to use this dashboard:", style = "font-family: GDS Transport, arial, sans-serif; font-weight: bold; font-size: 30px"),
          tags$ul(
            tags$li("Use the navigation bar on the left to select which outcome/enabler/indicator page you want to view"),
            tags$li("Within each outcome/enabler page, select a geographical level (National, Regional, Local authority) using the dropdown list at the top of each outcome and enabler page"),
            tags$li("When selecting regional or local authority, you will need to use the adjacent dropdowns and use the checkboxes to further filter the data. You can also, type the name directly into the box"),
            tags$li("Your selected geographical level will be shown above the first set of indicators for visibility"),
            tags$li("Most figures, charts and tables within each outcome/enabler page will update based on your geographic level selection except for regional Charts which won’t update. You need to select a new geographic level and new region/local authority for each outcome/enabler on the dashboard"),
            tags$li("Timeseries charts will update with extra lines when ‘Compare with national’ and/or ‘Compare with regional’ are selected"),
            tags$li("The domain (theme) options for each outcome/enabler page will be displayed below the drop downs. Select a different domain by clicking on the text. Note some outcome/enabler pages will only have one domain"),
            tags$li("All charts can be viewed as a table to view more detailed data at selected geographic levels, select ‘View chart as at table’ to view the table data for your geographic selection"),
            tags$li("Select ‘Additional information’ underneath charts and tables where available, to view footnotes regarding the data source and limitations for context"),
            tags$li("If you have selected ‘Local authority’ as the geographic level you can also select to view a chart called ‘10 statistical neighbours’"),
            style = "font-family: GDS Transport, arial, sans-serif; font-size: 19px;"
          )
        )
      ),
      gov_row(
        column(
          width = 12,
          tags$h3("Interactive plots user guide", style = "font-family: GDS Transport, arial, sans-serif; font-weight: bold; font-size: 30px"),
          p("The menu bar along the top of the charts contains extra interactive features to:"),
          tags$ul(
            tags$li("Download chart as png file"),
            tags$li("Hover over lines/bars in the plot to see specific values"),
            tags$li("Zoom in and out"),
            tags$li("Auto scale to provide a clearer view on trends"),
            tags$li("Reset axes back to the starting view"),
            tags$li("Compare data for each data point being hovered"),
            style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"
          ),
          tags$h4("Using the key", style = "font-family: GDS Transport, arial, sans-serif; font-size :19px; font-weight: bold"),
          tags$ul(
            tags$li("Double clicking a line/value in the key will isolate the value in the plot"),
            tags$li("Double clicking the same value again will restore the original plot"),
            tags$li("Single clicking a line/value in the key will remove that line/value from the plot"),
            style = "font-family: GDS Transport, arial, sans-serif; font-size :19px;"
          )
        )
      )
    )
  )
}
