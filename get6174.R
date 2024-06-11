library(gtools)
library(tibble)

# transform a vector of n digits to a number
# exemple : c(1, 2, 3, 4) -> 1234
get_number_from_vector <- function(vector) {
  
  l <- length(vector)
  power <- 10^((l - 1):0)
  return(sum(vector * power))
  
}

# create a matrix from a number with
# 1 row
# (power + 1) cols for each digit
# exemple :              [,1][,2][,3][,4]
#           1234 -> [1,]   1   2   3   4 
#                  
create_matrix_row_from_number <- function(number, power){
  
  matrice <- matrix(nrow = 1, ncol = power + 1)
  for (i in power:0) {
    x <- (number - (number %% 10^i)) / 10^i
    matrice[1, power - i + 1] <- x
    number <- number - x * 10^i
  }
  return(matrice)
  
}
#
# get diff between number issued from 
# digits ordered decreasingly
# and digits ordered increasingly
# 
# parameter : mat_digits 
# 
# matrice containing n digits unordered
# each digit in his own column
# 
get_diff_dec_inc_numbers <- function(mat_digits) {
  
  # sort decreasing all digits in all rows
  mat_digits_dec <- mat_digits
  for (i in 1:nrow(mat_digits_dec)) {
    mat_digits_dec[i,] <- sort(mat_digits_dec[i,], decreasing = TRUE)
  }
  # select unique rows
  mat_digits_dec <- unique(mat_digits_dec)
  # create matrice based on this last one
  # but with increasing digits
  mat_digits_inc <- mat_digits_dec
  for (i in 1:nrow(mat_digits_inc)) {
    mat_digits_inc[i,] <- sort(mat_digits_inc[i,], decreasing = FALSE)
  }
  vec_digits_dec <- apply(mat_digits_dec, 1, get_number_from_vector)
  vec_digits_inc <- apply(mat_digits_inc, 1, get_number_from_vector)
  vec_digits_diff <- unique(vec_digits_dec - vec_digits_inc)
  # transform differences into row digits matrices for another calculus stage
  mat_digits_diff <- t(
    sapply(vec_digits_diff, create_matrix_row_from_number, power = 3)
  )
  return(mat_digits_diff)
  
}

get_diff_dec_inc_numbers_v2 <- function(mat_digits) {

  mat_digits_dec <- mat_digits
  mat_digits_inc <- mat_digits
  for (i in 1:nrow(mat_digits_dec)) {
    mat_digits_dec[i,] <- sort(mat_digits[i,], decreasing = TRUE)
    mat_digits_inc[i,] <- sort(mat_digits[i,], decreasing = FALSE)
  }
  vec_digits_dec <- apply(mat_digits_dec, 1, get_number_from_vector)
  vec_digits_inc <- apply(mat_digits_inc, 1, get_number_from_vector)
  vec_digits_diff <- (vec_digits_dec - vec_digits_inc)
  # transform differences into row digits matrices for another calculus stage
  mat_digits_diff <- t(
    sapply(vec_digits_diff, create_matrix_row_from_number, power = 3)
  )
  return(mat_digits_diff)
  
}


#-------------------------------------------------------------------------------

# generate all combinations of length 4 from digits 0 to 9
# (10*9*8*7) / (4*3*2*1) = 210 rows


# 1st test with duplicate deletions
# 
mat_digits <- combinations(10, 4, 0:9)
repeat {
  mat_diff <- get_diff_dec_inc_numbers(mat_digits)
  print(mat_diff)
  if (identical(mat_diff, mat_digits)) {
    break
  }
  mat_digits <- mat_diff
}

# 2nd test without duplicate deletions
# 
mat_digits <- combinations(10, 4, 0:9)
mat_diff_results <- matrix(nrow = nrow(mat_digits), ncol = 8)
col <- 1
mat_diff_results[, col] <- apply(mat_digits, 1, get_number_from_vector)
col <- col + 1
repeat {
  mat_diff <- get_diff_dec_inc_numbers_v2(mat_digits)
  print(mat_diff)
  if (identical(mat_diff, mat_digits)) {
    break
  }
  mat_diff_results[, col] <- apply(mat_diff, 1, get_number_from_vector)
  col = col + 1
  mat_digits <- mat_diff
}
results <- tibble(mat_diff_results)

