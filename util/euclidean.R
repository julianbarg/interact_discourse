euclidean <- function(a, b){
  map2_dbl(a, b, ~sqrt(sum(unlist((.x - .y)^2))))
}
