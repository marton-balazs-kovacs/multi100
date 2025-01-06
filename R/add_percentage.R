# Function to calculate frequency and percentage
add_percentage <- function(data, count_var) {
  data |> 
    dplyr::mutate(
      N = sum({{count_var}}),
      freq = {{count_var}} / N,
      percentage = round(freq * 100, 2)
    )
}
