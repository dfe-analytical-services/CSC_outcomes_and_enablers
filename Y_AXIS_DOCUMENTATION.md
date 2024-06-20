# Y-Axis Documentation
### This document explains the rationale for some of the y-axis dynamic scales.

## plotting.R
### plotly_time_series_custom_scale
In this function, there is a line of code that reads: `ylim_upper <- (ceiling(ylim_upper / 20) * 20) + (ylim_upper * 0.05)`.\
\
There are many instances in server.R where this function is called to output a time series line chart. Each instance **must** pass through a `ylim_upper` variable for use in the line of code above. The line of code performs the following:

1. `ceiling(ylim_upper / 20) * 20` divides the `ylim_upper` value by 20, then `ceiling` ensures the divided value is always rounded up to the nearest interger (e.g. 3.4 would become 4). Then, the resulting integer is multiplied by 20.

2. `ylim_upper * 0.05` gets 5% of the ylim_upper value.

3. Both values are then added together to get the final `ylim_upper` value.

The ylim_upper value is then used to set the max y-axis value: 
```r
scale_y_continuous(
        limits = c(0, ylim_upper)
      ) +
```

Essentially, this line of code is getting the 105% value of the value of ylim_upper after it has been rounded up to the nearest 20.\
\
The rounding up to the nearest 20 aims to allow ggplot2 to more easily set breaks in the y-axis scale. The 105% value aims to allow the top of the y-axis to have a number tick value, as it can sometimes remove this if it is too close to the max value of the y-axis scale.

### by_la_bar_plot
This function works in a similar way, but the variable here is name yupperlim and is **not** mandatory. This is the first line of the function:
```r
by_la_bar_plot <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, yupperlim = NULL, add_rect = FALSE) {
```
The `yupperlim = NULL` signals that the yupperlim value will be set to NULL unless a value is passed when the function is called in server.R.\
\
The function then uses an if/else loop to decide what to do with the `yupper_lim` value:
```r
    if (is.null(yupperlim)) {
      p <- p + scale_y_continuous(limits = c(0, 100))
    } else {
      # Set the upper limit of the y-axis, then give it a bit extra on top of that so the max y-axis tick has a better chance of being near the top of the axis
      yupperlim <- (ceiling(yupperlim / 10) * 10) + (yupperlim * 0.05)
      p <- p + scale_y_continuous(limits = c(0, yupperlim))
    }
```
If there is a `yupper_lim` value passed, a similar line of code to the **plotly_time_series_custom_scale** function will be executed, but rounding to the nearest 10 above instead of 20. The reason the time series needs 20 is so that the circular data points are not cut off by the top of the graph area, so needs a slight buffer, whereas the bar charts are flat and
do not suffer from this.

### by_region_bar_plot
This function works the same as the LA bar plot, but yupperlim is a mandatory variable.\
\
The yupperlim is given the a same "ceiling" and rounding to nearest 10 plus 5% treatment and then used as the maxvalue for the y-axis scale.

### statistical_neighbours_plot
This function is called for almost all of the statisitcal neighbours bar charts that exist on the dashboard.\
\
It follows the same method of `ylim_upper <- (ceiling(ylim_upper / 10) * 10) + (ylim_upper * 0.05)`. It should match the scale for the normal LA bar chart, but this is done on the server.R side.

### Misc. Functions
There are some functions specific to one chart, such as **plot_uasc**. The reason for these are that they require a slightly different format than the other time series, region, or LA bar charts.\
\
Using **plot_uasc** as an example, it is a stacked bar chart, so would need to take in different variable than a normal bar chart. However, as seen in the image below, there is still a `max_rate` calculated and used in the same way yupperlim is calculated and used, so the methodology remains the same.
```r
# Set the max y-axis scale based on the data
  max_rate <- max(
    combined_cla_data$`Placement Rate Per 10000`[combined_cla_data$population_count == "Children starting to be looked after each year" &
      combined_cla_data$characteristic %in% c("Unaccompanied asylum-seeking children", "Non-unaccompanied asylum-seeking children")],
    na.rm = TRUE
  )

  # Round the max_rate to the nearest 20 then multiply by 1.05 (this will be used for the upper y-axis limit)
  max_rate <- (ceiling(max_rate / 20) * 20) + (max_rate * 0.05)
```
\
The list of unique functions that use the dynamic y-axis method in plotting.R are as follows:

- plot_uasc

- plot_uasc_reg

- plot_uasc_la

- plot_cla_rate_reg

- plot_cla_rate_la

- plot_cla_march_reg

- plot_cla_march_la

- plot_cin_rate_reg

- plot_cin_rate_la

- plot_cin_referral_reg

- plot_cin_referral_la

- statistical_neighbours_plot_uasc

## server.R
When calling one of the functions above in server.R, a `yupperlim` value needs to be passed through so the rounding can be performed. In most cases, this is in the form of a `max_rate`, example below:
```r
    # Set the max y-axis scale
    max_rate <- max(workforce_data$`Caseload Fte`, na.rm = TRUE)

    # Round the max_rate to the nearest 20
    max_rate <- ceiling(max_rate / 20) * 20

    p <- plotly_time_series_custom_scale(filtered_data, input$select_geography_e3, input$geographic_breakdown_e3, "Caseload Fte", "Average caseload (FTE)", max_rate) %>%
      config(displayModeBar = F)
    p <- p + ggtitle("Average caseload (FTE)")
```
***It is important that the max_rate is calculated from the original dataset and NOT the filtered_data that appears in most server.R instances calling a plotting.R function. This is because the orignal dataset will have a fixed maximum value, whereas the filtered_data chanages depending on the dashboard dropdown
selections.***

### Calling region or LA functions
When calling region or LA plot functions, remember to filter the original dataset on the regional or LA level data. Also, if the chart only shows the latest year data, filter the dataset to the latest year when defining the max_rate variable. For example:
```r
    max_rate <- max(workforce_data$`Caseload Fte`[workforce_data$time_period == max(workforce_data$time_period) &
                                                      workforce_data$geographic_level == "Regional"], na.rm = TRUE)

    max_rate <- ceiling(max_rate / 10) * 10
```
***Remember to also filter the original dataset on certain characteristics if required. Example below:***
```r
    # Set the max y-axis scale
    max_rate <- max(placement_changes_data$`Percent`[placement_changes_data$placement_stability == "With 3 or more placements during the year"], na.rm = TRUE)

    max_rate <- ceiling(max_rate / 20) * 20
```

### Addressing known outliers
In cases where large outliers are known, they have been hard coded to be excluded from the max_rate calculation unless they are chosen specifically in the dashboard dropdown. For example in the CLA rates time series:
```r
    if (input$geographic_breakdown_o1 == "City of London") {
      # Set the max y-axis scale with City of London
      max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 50) * 50
    } else {
      # Set the max y-axis scale without City of London
      max_rate <- max(cla_rates$`Rate Per 10000`[cla_rates$population_count == "Children starting to be looked after each year" & cla_rates$geo_breakdown != "City of London"], na.rm = TRUE)
      max_rate <- ceiling(max_rate / 20) * 20
    }
```
