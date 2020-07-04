CV <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  if(any(x <= 0))
    stop("Your data must be positive!")
  sd(x)/mean(x)
}

medCV <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  if(any(x <= 0))
    stop("Your data must be positive!")
  mad(x)/median(x)
}

iqrCV <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  if(any(x <= 0))
    stop("Your data must be positive!")
  sIQR(x)/median(x)
}
