illustrate.quantile <- function(x, alpha, type){
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(alpha))
  stopifnot(length(alpha) == 1)
  stopifnot(0 < alpha && alpha < 1)

  n <- length(x)
  x.ord <- sort(x)
  if(abs(n*alpha - as.integer(n*alpha)) < 1e-16){
    ind <- as.integer(n*alpha)
    quant <- c(x.ord[ind], x.ord[ind+1])
  }else{
    quant <- x.ord[ceiling(n*alpha)]
  }
  DFq <- data.frame(quant = quant, y = 0)
  DF <- data.frame(x = x, y = 0)
  if(missing(type)) type <- 1:9
  if(length(type) > 1){
    Qres <- numeric(length(type))
    for(i in 1:length(type)){
      Qres[i] <- quantile(x, type = i, probs = alpha)
    }
    DFqR <- data.frame(x = Qres, y = 0)
  }else{
    DFqR <- data.frame(x = quantile(x, type = type, probs = alpha), y = 0)
  }
  gg <- ggplot(DF, aes(x = .data$x, y = .data$y)) +
    theme_minimal() + 
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    ylim(-0.2, 0.2) + geom_point() + geom_hline(yintercept = 0) +
    geom_point(data = DFq, aes(x = .data$quant, y = .data$y), 
               color = "#CC0C00", pch = "|", size = 10, 
               position = position_nudge(y=0.02)) +
    ggtitle(paste0("Illustration of ", alpha, "-Quantile"))
  if(length(type) > 1){
    gg <- gg + geom_point(data = DFqR, aes(x = .data$x, y = .data$y), 
                          pch = 8, size = 3, color = "#5C88DA",
                          position = position_nudge(y = -0.01)) +
      annotate(geom = "text", x = (max(DFqR$x)+min(DFqR$x))/2, y = -0.03, 
               label = "results of function quantile", color = "#5C88DA")
  }else{
    gg <- gg + geom_point(data = DFqR, aes(x = .data$x, y = .data$y), pch = 8, size = 3, 
                          color = "#5C88DA") +
      annotate(geom = "text", x = DFqR$x, y = -0.03,
               label = paste0("result of function quantile\n(type = ", type, ")"), 
                             color = "#5C88DA")
  }
  if(length(quant) == 2){
    gg <- gg + geom_line(data = DFq, aes(x = .data$quant, y = .data$y), 
                         color = "#CC0C00", position = position_nudge(y = 0.02)) +
      annotate(geom = "text", x = mean(quant), y = 0.05, 
               label = "population quantile", color = "#CC0C00")
  }else{
    gg <- gg + annotate(geom = "text", x = quant, y = 0.05, 
                        label = "population quantile", color = "#CC0C00")
  }
  print(gg)
  invisible(gg)
}
