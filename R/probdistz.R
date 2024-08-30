#' Probability Distribution and Z-Score Calculation Function
#'
#' @description
#' The `probdistz` function calculates the probability density function (PDF), cumulative density function (CDF), and Z-scores for specified yield columns in a dataset, optionally generating smoothed data.
#'
#' @param data A data frame containing the dataset to be analyzed.
#' @param env_cols A character vector of column names representing environmental variables in the dataset. These columns will be treated as factors.
#' @param yield_cols A character vector of column names representing yield variables in the dataset. These columns will be treated as numeric.
#' @param smooth A logical value indicating whether to generate smoothed PDF, CDF, and Z-scores. If `TRUE`, the function will generate smoothed values over a range of the yield data. Defaults to `TRUE`.
#'
#' @details
#' This function processes the input dataset by first ensuring that the specified environmental and yield columns exist and are in the correct format. It then generates all unique combinations of the environmental variables and calculates the PDF, CDF, and Z-scores for each combination and yield column. If `smooth` is `TRUE`, the function generates smoothed data over a specified range of the yield values.
#'
#' The function returns a data frame containing the calculated values, including the original environmental variables and yield values, as well as the calculated PDF, CDF, and Z-scores for each combination of environmental variables.
#'
#' @return A data frame containing the calculated PDF, CDF, and Z-scores, with optional smoothed values. The data frame includes the original environmental variables and yield values, along with the new calculated columns.
#'
#' @examples
#' # Example usage:
#' # Assuming `data` is your dataset, `env_cols` are your environmental columns,
#' # and `yield_cols` are your yield columns:
#' result= probdistz(data = your_data, env_cols = c("env1", "env2"), yield_cols = c("yield1", "yield2"))
#'
#' @export

probdistz= function(data, env_cols, yield_cols, smooth = TRUE) {
  # Ensure the necessary packages are installed and loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", dependencies = TRUE)
  if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats", dependencies = TRUE)
  library(dplyr)
  library(stats)

  # Check if the specified columns exist in the dataset
  missing_cols= setdiff(c(env_cols, yield_cols), colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Columns not found in the dataset:", paste(missing_cols, collapse = ", ")))
  }

  # Ensure the columns are in the correct format
  data[env_cols]= lapply(data[env_cols], as.factor)
  data[yield_cols]= lapply(data[yield_cols], as.numeric)

  # Initialize an empty dataframe to store the final output
  final_data= data.frame()

  # Generate all unique combinations of the environmental columns
  unique_combinations= unique(data[env_cols])

  for (yield_col in yield_cols) {
    for (i in 1:nrow(unique_combinations)) {
      # Extract the current combination
      env_comb= unique_combinations[i, , drop = FALSE]

      # Filter data for the current combination of environmental variables
      filtered_data= data
      for (col in env_cols) {
        filtered_data= filtered_data %>% filter(!!sym(col) == env_comb[[col]])
      }

      # Calculate PDF, CDF, and Z-score for the actual data
      filtered_data= filtered_data %>%
        mutate(
          !!paste0("pdf_", yield_col) := dnorm(!!sym(yield_col), mean = mean(!!sym(yield_col),
                                                                             na.rm = TRUE), sd = sd(!!sym(yield_col), na.rm = TRUE)),
          !!paste0("cdf_", yield_col) := pnorm(!!sym(yield_col), mean = mean(!!sym(yield_col),
                                                                             na.rm = TRUE), sd = sd(!!sym(yield_col), na.rm = TRUE)),
          !!paste0("z_", yield_col) := (!!sym(yield_col) - mean(!!sym(yield_col), na.rm = TRUE)) / sd(!!sym(yield_col),
                                                                                                      na.rm = TRUE)
        )

      if (smooth) {
        summary_stats= filtered_data %>%
          summarize(mean = mean(!!sym(yield_col), na.rm = TRUE),
                    sd = sd(!!sym(yield_col), na.rm = TRUE))

        smooth_data= summary_stats %>%
          rowwise() %>%
          do({
            mean_val= .$mean
            sd_val= .$sd

            if (is.na(sd_val) || sd_val == 0) {
              return(data.frame())
            }

            min_value= mean_val - 6 * sd_val
            max_value= mean_val + 6 * sd_val
            x_values= seq(min_value, max_value, length.out = 1000)

            # Create the smooth_data dataframe
            smooth_data= data.frame(
              yield_col_value= x_values,
              smooth_pdf= dnorm(x_values, mean = mean_val, sd = sd_val),
              smooth_cdf= pnorm(x_values, mean = mean_val, sd = sd_val),
              smooth_z= (x_values - mean_val) / sd_val
            )

            # Rename yield_col_value to the actual yield column name
            colnames(smooth_data)[1]= yield_col

            # Add environmental columns back to smooth_data
            for (col in env_cols) {
              smooth_data[[col]]= rep(env_comb[[1, col]], each = length(x_values))
            }

            smooth_data
          }) %>%
          ungroup() %>%
          mutate(across(all_of(env_cols), as.factor)) # Ensure the columns are factors in the final output

        final_data= bind_rows(final_data, smooth_data)
      } else {
        final_data= bind_rows(final_data, filtered_data)
      }
    }
  }

  return(final_data)
}
