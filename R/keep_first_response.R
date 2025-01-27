#' Keep the first response
#' 
#' @description The function checks for multiple responses with a given id and flags and keeps only
#' the first response based on a time variable.
keep_first_response <- function(data, id_var, time_var) {
  data %>%
    dplyr::group_by({{id_var}}) %>% 
    dplyr::mutate(
      first_response = dplyr::if_else(
        {{time_var}} == min({{time_var}}),
        TRUE, FALSE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(first_response)
}