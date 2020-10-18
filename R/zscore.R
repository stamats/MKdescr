zscore <- function(x, na.rm = FALSE){
  (x - mean(x, na.rm = na.rm))/sd(x, na.rm = na.rm)
}
medZscore <- function(x, na.rm = FALSE, constant = 1/qnorm(0.75)){
  (x - median(x, na.rm = na.rm))/mad(x, na.rm = na.rm, constant = constant)
}
iqrZscore <- function(x, na.rm = FALSE, type = 7, constant = 2*qnorm(0.75)){
  (x - median(x, na.rm = na.rm))/sIQR(x, na.rm = na.rm, type = type, constant = constant)
}
