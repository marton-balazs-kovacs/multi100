#'  Save codebook with standard variable names
save_codebook <- function(codebook, output_path) {
  # Create the full path using `here::here` and the default directory
  # Process the codebook and save it
  codebook |> 
    dplyr::rename(
      variable_name = new_names,
      description = variable_names
    ) |> 
    readr::write_csv(output_path)
}
