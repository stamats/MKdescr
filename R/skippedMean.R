##Huber-type skipped mean
skippedMean <- function(x, na.rm = FALSE, constant = 3.0){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  stopifnot(constant > 0, length(constant) == 1)
  
  M <- median(x)
  MAD <- mad(x)
  mean(x[M - constant*MAD < x & x < M + constant*MAD])
}
skippedSD <- function(x, na.rm = FALSE, constant = 3.0){
  stopifnot(is.numeric(x))
  if(na.rm) x <- x[!is.na(x)]
  stopifnot(constant > 0, length(constant) == 1)
  
  M <- median(x)
  MAD <- mad(x)
  sd(x[M - constant*MAD < x & x < M + constant*MAD])
}
