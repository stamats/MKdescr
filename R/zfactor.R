zfactor <- function(x, y, na.rm = FALSE){
  mx <- mean(x, na.rm = na.rm)
  my <- mean(y, na.rm = na.rm)
  sx <- sd(x, na.rm = na.rm)
  sy <- sd(y, na.rm = na.rm)
  1 - 3*(sx + sy)/abs(mx - my)
}
medZfactor <- function(x, y, na.rm = FALSE, constant = 1/qnorm(0.75)){
  mx <- median(x, na.rm = na.rm)
  my <- median(y, na.rm = na.rm)
  sx <- mad(x, na.rm = na.rm, constant = constant)
  sy <- mad(y, na.rm = na.rm, constant = constant)
  1 - 3*(sx + sy)/abs(mx - my)
}
iqrZfactor <- function(x, y, na.rm = FALSE, type = 7, constant = 2*qnorm(0.75)){
  mx <- median(x, na.rm = na.rm)
  my <- median(y, na.rm = na.rm)
  sx <- sIQR(x, na.rm = na.rm, type = type, constant = constant)
  sy <- sIQR(y, na.rm = na.rm, type = type, constant = constant)
  1 - 3*(sx + sy)/abs(mx - my)
}
