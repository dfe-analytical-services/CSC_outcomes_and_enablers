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
```
scale_y_continuous(
        limits = c(0, ylim_upper)
      ) +
```

Essentially, this line of code is getting the 105% value of the value of ylim_upper after it has been rounded up to the nearest 20.\
\
The rounding up to the nearest 20 aims to allow ggplot2 to more easily set breaks in the y-axis scale. The 105% value aims to allow the top of the y-axis to have a number tick value, as it can sometimes remove this if it is too close to the max value of the y-axis scale.

### by_la_bar_plot
This function works in a similar way, but the variable here is name yupperlim and is **not** mandatory. This is the first line of the function:
```
by_la_bar_plot <- function(dataset, selected_geo_breakdown = NULL, selected_geo_lvl = NULL, yvalue, yaxis_title, yupperlim = NULL, add_rect = FALSE) {
```
The `yupperlim = NULL` signals that the yupperlim value will be set to NULL unless a value is passed when the function is called in server.R.\
\
The function then uses an if/else loop to decide what to do with the `yupper_lim` value:
```
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
Using **plot_uasc** as an example, it is a stacked bar chart, so would need to take in different variable than a normal bar chart. However, as seen in the image below, there is still a `max_rate` calculated and used in the same way yupperlim is calculated and used, so the methodology remains the same.\
\
![Screenshot 2024-06-19 092451](https://github.com/dfe-analytical-services/CSC_outcomes_and_enablers/assets/148988846/428d4b98-215b-4994-b138-1ed906d9da15)\
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







- server.R: ceiling rate also there for a fail safe (will calc the same anyway).
- server.R: Isle of Scilly and CoL scenarios.
