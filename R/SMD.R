SMD <- function(x, y, bias.cor = TRUE, var.equal = FALSE, na.rm = FALSE){
  AMx <- mean(x, na.rm = na.rm)
  AMy <- mean(y, na.rm = na.rm)
  VARx <- var(x, na.rm = na.rm)
  VARy <- var(y, na.rm = na.rm)
  if(na.rm){
    nx <- length(x[!is.na(x)])
    ny <- length(y[!is.na(y)])
  }else{
    nx <- length(x)
    ny <- length(y)
  }
  if(var.equal){
    VAR <- ((nx-1)*VARx + (ny-1)*VARy)/(nx+ny-2)
    SMD <- (AMx - AMy)/sqrt(VAR)
    if(bias.cor){
      df <- nx+ny-2
      J <- lgamma(df/2)/(sqrt(df/2)*gamma((df-1)/2))
      SMD <- exp(J)*SMD
    }
  }else{
    VAR <- VARx/nx + VARy/ny
    SMD <- (AMx-AMy)/sqrt(VAR)/sqrt(nx*ny/(nx+ny))
    if(bias.cor){
      df <- (VARx/nx + VARy/ny)^2/(VARx^2/(nx^2*(nx-1)) + VARy^2/(ny^2*(ny-1)))
      J <- lgamma(df/2)/(sqrt(df/2)*gamma((df-1)/2))
      SMD <- exp(J)*SMD
    }
  }
  if(bias.cor){
    names(SMD) <- "bias-corrected SMD"
  }else{
    names(SMD) <- "SMD"
  }
  SMD
}
