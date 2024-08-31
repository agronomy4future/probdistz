#' Calculate Probability Distributions (PDF, CDF, and Z-Scores) with Optional Smoothing
#'
#' This function calculates the Probability Density Function (PDF), Cumulative Distribution Function (CDF), and Z-scores for the given yield columns within specified environmental factor groups. Optionally, smoothed curves can be generated based on normal distribution assumptions.
#'
#' @param data A data frame containing the dataset to be analyzed.
#' @param env_cols A character vector specifying the names of the environmental factor columns in the data frame.
#' @param yield_cols A character vector specifying the names of the yield columns for which the PDF, CDF, and Z-scores will be calculated.
#' @param smooth A logical value indicating whether to generate smoothed PDF, CDF, and Z-scores based on normal distribution assumptions. Default is \code{TRUE}.
#'
#' @return A data frame containing the original data along with additional columns for the calculated PDF, CDF, and Z-scores. If \code{smooth = TRUE}, additional columns with smoothed values are included.
#'
#' @examples
#' # Sample data
#' df= data.frame(
#'                field = c("A", "A", "B", "B"),
#'                genotype = c("X", "Y", "X", "Y"),
#'                grain_weight = c(45, 50, 55, 60)
#' )
#'
#' # Calculate probability distributions with smoothing
#'    output= probdistz(df, env_cols= c("field", "genotype"), yield_cols= c("grain_weight"), smooth=TRUE or FALSE)
#'
#'    * smooth= False → to calculate PDF, CDF, and Z-Scores with actual data set
#'    ** smooth= False → to calculate PDF, CDF, and Z-Scores with extended data set until 6σ
#'
#' # View result
#' head(result)
#'
#' @export
#'

probdistz= function(data, env_cols, yield_cols, smooth= TRUE) {
  # Ensure the necessary packages are installed and loaded
  if (!requireNamespace("dplyr", quietly= TRUE)) install.packages("dplyr", dependencies= TRUE)
  if (!requireNamespace("stats", quietly= TRUE)) install.packages("stats", dependencies= TRUE)
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
        filtered_data= filtered_data %>% filter(.data[[col]]== env_comb[[1, col]])
      }

      # Calculate PDF, CDF, and Z-score for the actual data
      filtered_data= filtered_data %>%
        mutate(
          !!paste0("pdf_", yield_col) := dnorm(.data[[yield_col]], mean= mean(.data[[yield_col]], na.rm= TRUE), sd= sd(.data[[yield_col]], na.rm= TRUE)),
          !!paste0("cdf_", yield_col) := pnorm(.data[[yield_col]], mean= mean(.data[[yield_col]], na.rm= TRUE), sd= sd(.data[[yield_col]], na.rm= TRUE)),
          !!paste0("z_", yield_col) := (.data[[yield_col]] - mean(.data[[yield_col]], na.rm= TRUE)) / sd(.data[[yield_col]], na.rm= TRUE)
        )

      if (smooth) {
        summary_stats= filtered_data %>%
          summarize(mean = mean(.data[[yield_col]], na.rm= TRUE),
                    sd = sd(.data[[yield_col]], na.rm= TRUE))

        smooth_data= summary_stats %>%
          rowwise() %>%
          do({
            mean_val= .$mean
            sd_val= .$sd

            if (is.na(sd_val) || sd_val== 0) {
              return(data.frame())
            }

            min_value= mean_val - 6 * sd_val
            max_value= mean_val + 6 * sd_val
            x_values= seq(min_value, max_value, length.out= 1000)

            # Create the smooth_data dataframe
            smooth_data= data.frame(
              yield_col_value= x_values,
              smooth_pdf= dnorm(x_values, mean= mean_val, sd= sd_val),
              smooth_cdf= pnorm(x_values, mean= mean_val, sd= sd_val),
              smooth_z= (x_values - mean_val) / sd_val
            )

            # Rename yield_col_value to the actual yield column name
            colnames(smooth_data)[1]= yield_col

            # Add environmental columns back to smooth_data
            for (col in env_cols) {
              smooth_data[[col]]= rep(env_comb[[1, col]], each= length(x_values))
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
