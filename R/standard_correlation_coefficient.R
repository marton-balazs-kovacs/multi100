#' Calculate standard correlation coefficient
standard_correlation_coefficient <- function(type_of_statistic, test_statistic, sample_size, df1, df2, reanalysis_es_missing) {
  # Only calculate the correlation coefficient for cases where all the values needed are there
  if(reanalysis_es_missing == 1) {
    return(NA_real_)
  }
  
  result <- tryCatch(
    {
      case_result <- NA_real_  # Default result if none of the conditions match
      if (type_of_statistic == "z") {
        case_result <- tanh(abs(test_statistic)*sqrt(1/(sample_size-3)))*sign(test_statistic)
      } else if (type_of_statistic == "chi2") {
        case_result <- tanh(sqrt(abs(test_statistic))*sqrt(1/(sample_size-3)))*sign(test_statistic)
      } else if (type_of_statistic == "t") {
        case_result <- sqrt(abs(test_statistic)^2/(test_statistic^2+df1))*sign(test_statistic)
      } else if (type_of_statistic == "F") {
        case_result <- sqrt(abs(sqrt(test_statistic))^2/(sqrt(test_statistic)^2+df2))*sign(test_statistic)
      } else if (type_of_statistic == "r") {
        case_result <- test_statistic
      } else if (type_of_statistic == "tau") {
        case_result <- sin(.5 * test_statistic * pi)
      }
      case_result
    },
    warning = function(w) {
      message("Warning in standard_correlation_coefficient with input parameters:")
      message(paste("Type of statistic:", type_of_statistic))
      message(paste("Test statistic:", test_statistic))
      message(paste("Sample size:", sample_size))
      message(paste("df1:", df1))
      message(paste("df2:", df2))
      message("Warning message: ", w$message)
      NA_real_  # Return NA in case of warning
    },
    error = function(e) {
      message("Error in standard_correlation_coefficient with input parameters:")
      message(paste("Type of statistic:", type_of_statistic))
      message(paste("Test statistic:", test_statistic))
      message(paste("Sample size:", sample_size))
      message(paste("df1:", df1))
      message(paste("df2:", df2))
      message("Error message: ", e$message)
      NA_real_  # Return NA in case of error
    }
  )
  
  return(result)
}