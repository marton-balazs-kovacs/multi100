# Function to compute standard error of d from r and sample size n
se_d <- function(r, n) {
  2 / (sqrt(n - 3) * sqrt(1 - r^2))
}
