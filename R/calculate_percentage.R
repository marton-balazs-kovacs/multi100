#' Function to calculate the count and percentage of the responses for a response variable
calculate_percentage <- function(data, response_var) {
  data %>% 
    dplyr::count({{response_var}}) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      N = sum(n),
      freq = round(n / N, 4),
      percentage = freq * 100
    ) 
}