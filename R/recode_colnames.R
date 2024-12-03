#' Recode column names in dataset based on codebook
recode_colnames <- function(data, codebook) {
  colnames(data) <- dplyr::recode(
    colnames(data),
    !!!setNames(
      as.character(codebook$new_names),
      codebook$variable_names
    )
  )
  data
}
