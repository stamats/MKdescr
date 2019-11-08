simCorVars <- function(n, r, mu1=0, mu2=0, sd1=1, sd2=1, plot = TRUE){
  if(!is.integer(n)) n <- as.integer(n)
  if(length(n) > 1){
    n <- n[1]
    warning("length(n) > 1: only the first entry is used.")
  }
  stopifnot(is.numeric(r))
  stopifnot(abs(r) <= 1)
  if(length(r) > 1){
    r <- r[1]
    warning("length(r) > 1: only the first entry is used.")
  }
  stopifnot(is.numeric(mu1), length(mu1) == 1, 
            is.numeric(mu2), length(mu2) == 1,
            is.numeric(sd1), length(sd1) == 1, 
            is.numeric(sd2), length(sd1) == 1)
  x1 <- rnorm(n)
  y1 <- rnorm(n)
  a <- sqrt((1-r)/(1+r))
  if(abs(r) == 1){
    x <- x1*sd1 + mu1
    y <- sign(r)*x1*sd2 + mu2
  }else{
    x <- (x1 + a*y1)*sd1 + mu1
    y <- (x1 - a*y1)*sd2 + mu2
  }
  if(plot) plot(x, y, main = paste("Correlation =", r),
                xlab = "Variable 1", ylab = "Variable 2")
  data.frame(Var1 = x, Var2 = y)
}
