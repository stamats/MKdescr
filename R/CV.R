CV <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  if(any(x <= 0))
    stop("Your data must be positive!")
  sd(x)/mean(x)
}

medCV <- function(x, na.rm = FALSE, constant = 1/qnorm(0.75)){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  if(any(x <= 0))
    stop("Your data must be positive!")
  mad(x, constant = constant)/median(x)
}

iqrCV <- function(x, na.rm = FALSE, type = 7, constant = 2*qnorm(0.75)){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  if(any(x <= 0))
    stop("Your data must be positive!")
  sIQR(x, type = type, constant = constant)/median(x)
}
