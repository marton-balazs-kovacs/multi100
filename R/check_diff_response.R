#' Function to check different response to a question in case of multiple analysis
check_diff_response <- function(data, id_var, response_var) {
  data %>% 
    dplyr::count({{id_var}}, {{response_var}}) %>% 
    dplyr::group_by({{id_var}}) %>% 
    dplyr::mutate(n_per_analyst = n()) %>% 
    dplyr::arrange(desc(n_per_analyst)) %>%
    dplyr::filter(n_per_analyst > 1L)
}