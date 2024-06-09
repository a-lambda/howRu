library(gtools)
library(p)
# generate all sequences of length 4 from digits 0 to 9
mat_4d <- permutations(10, 4, 0:9)
list_4d <- apply(mat_4d, 1, list)
# sort every element decreasing

mat_dec_4_digits <- lapply(list_4d, sort, decreasing = TRUE)
mat_inc_4_digits <- apply(mat_4_digits, 1, sort)

# filter only those where figures are decreasing
# 
get_number_from_vector <- function(vector) {
  l <- length(vector)
  power <- 10^((l - 1):0)
  return(sum(vector * power))
}


