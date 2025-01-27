get_comparison_fn <- function(operator) {
  switch(operator,
         ">=" = function(a, b) a >= b,
         "<=" = function(a, b) a <= b,
         ">" = function(a, b) a > b,
         "<" = function(a, b) a < b,
         "==" = function(a, b) a == b,
         stop("Invalid operator")
  )
}
