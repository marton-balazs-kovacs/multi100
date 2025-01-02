#' High-level function for counting and calculating percentage
calculate_percentage <- function(data, response_var) {
  data %>% 
    dplyr::count({{response_var}}) %>% 
    dplyr::ungroup() %>% 
    add_percentage(n)
}