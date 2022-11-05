sevenNS <- function(x, na.rm = TRUE, type = 7){
  xna <- is.na(x)

  if (na.rm) 
    x <- x[!xna]
  else if (any(xna)) 
    return(rep.int(NA, 7))

  n <- length(x)
  if (n == 0) 
    return(rep.int(NA, 7))
  else{
    return(c('Minimum' = min(x, na.rm = na.rm), 
             '1.Octile' = quantile(x, prob = 0.125, type = type, na.rm = na.rm),
             '1.Quartile' = quantile(x, prob = 0.25, type = type, na.rm = na.rm),
             'Median' = median(x, na.rm = na.rm),
             '3.Quartile' = quantile(x, prob = 0.75, type = type, na.rm = na.rm),
             '7.Octile' = quantile(x, prob = 0.875, type = type, na.rm = na.rm),
             'Maximum' = max(x, na.rm = na.rm)))
  }
}
