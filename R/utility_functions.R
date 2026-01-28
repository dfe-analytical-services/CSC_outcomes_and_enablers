# function to calculate the max rate for the y-axis
calculate_max_rate <- function(dataset, column_name, ceiling_adjustment = 20) {
  max_rate <- max(dataset[[column_name]], na.rm = TRUE)
  max_rate <- ceiling(max_rate / ceiling_adjustment) * ceiling_adjustment
}
