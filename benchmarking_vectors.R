library(microbenchmark)
library(tidyverse)
library(purrr)
library(Rcpp)

vector_grow_c <- function(x) {
  
  output <- numeric()
  for (i in 1:x) {
    output <- c(output, i^2)
  }
  return(output)
  
}

vector_grow_br <- function(x) {
  
  output <- numeric()
  for (i in 1:x) {
    output[i] <- i^2
  }
  return(output)
  
}

vector_prealloc <- function(x) {
  
  output <- numeric(x)
  for (i in 1:x) {
    output[i] <- i^2
  }
  return(output)
  
}

vector_colon <- function(x) {
  
  output <- (1:x)^2
  return(output)

}

vector_seq <- function(x) {
  
  output <- (seq(1, x,)^2)
  return(output)

}
  
vector_sapply <- function(x) {
  
  output <- sapply(1:x, \(x) x^2)
  return(output)
  
}

vector_map <- function(x) {
  
  output <- 1:x |> 
    map(\(x) x^2)
  return(output)
  
}


sourceCpp("benchmarking.cpp")

n <- 1e4

microbenchmark(
  vector_grow_c(n), 
  vector_grow_br(n),
  vector_prealloc(n),
  vector_colon(n),
  vector_seq(n),
  vector_sapply(n),
  vector_map(n),
  vector_rcpp(n),
  vector_grow_c(n/100), 
  vector_grow_br(n/100),
  vector_prealloc(n/100),
  vector_colon(n/100),
  vector_seq(n/100),
  vector_sapply(n/100),
  vector_map(n/100),
  vector_rcpp(n/100)
) |> 
  summarize(median_time = median(time), .by = expr) |> 
  arrange(median_time)
