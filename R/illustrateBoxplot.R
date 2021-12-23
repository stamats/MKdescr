illustrate.boxplot <- function(x){
  DF <- data.frame(x = x, y = 1)
  iqr <- quantile(x, probs = c(0.25, 0.75))
  DFiqr <- data.frame(x = iqr, y = 1.5)
  DFmin <- data.frame(x = c(min(x), iqr[1]), y = 1.5)
  DFmax <- data.frame(x = c(iqr[2], max(x)), y = 1.5)
  DFwhisker <- data.frame(x = c(iqr[1]-1.5*diff(iqr),
                                iqr[2]+1.5*diff(iqr)), y = 1.75)
  DFout1 <- data.frame(x = c(min(x, iqr[1]-2.5*diff(iqr)), 
                             iqr[1]-1.5*diff(iqr)), y = 1.75)
  DFout2 <- data.frame(x = c(iqr[2]+1.5*diff(iqr), 
                             max(x, iqr[2]+2.5*diff(iqr))), y = 1.75)
  x.ok <- x[x >= iqr[1]-1.5*diff(iqr) & x <= iqr[2]+1.5*diff(iqr)]
  low.fence <- max(min(x.ok), iqr[1]-1.5*diff(iqr))
  upp.fence <- min(max(x.ok), iqr[2]+1.5*diff(iqr))
  gg <- ggplot(DF, aes_string(x = "x", y = "y")) +
    theme_minimal() + 
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    geom_boxplot() + ylim(0, 2) + 
    geom_vline(xintercept = iqr[1]-1.5*diff(iqr), linetype = "dotted",
               color = "#CC0C00") + 
    geom_vline(xintercept = iqr[2]+1.5*diff(iqr), linetype = "dotted",
               color = "#CC0C00") + 
    geom_vline(xintercept = low.fence, linetype = "dotted",
               color = "#5C88DA") + 
    geom_vline(xintercept = upp.fence, linetype = "dotted",
               color = "#5C88DA") + 
    geom_point(aes_string(x = "x", y = "y"), data = DFiqr, pch = "|", size = 10, 
               position = position_nudge(y=0.03), color = "#CC0C00") +
    geom_line(data = DFiqr, aes_string(x = "x", y = "y"), 
              color = "#CC0C00", position = position_nudge(y = 0.02)) +
    annotate(geom = "text", x = mean(iqr), y = 1.6, label = "middle 50%",
             color = "#CC0C00") +
    geom_point(aes_string(x = "x", y = "y"), data = DFmin, pch = "|", size = 10, 
               position = position_nudge(y=0.03), color = "#CC0C00") +
    geom_line(data = DFmin, aes_string(x = "x", y = "y"), 
              color = "#CC0C00", position = position_nudge(y = 0.02)) +
    annotate(geom = "text", x = mean(DFmin$x), y = 1.6, label = "lower 25%",
             color = "#CC0C00") +
    geom_point(aes_string(x = "x", y = "y"), data = DFmax, pch = "|", size = 10, 
               position = position_nudge(y=0.03), color = "#CC0C00") +
    geom_line(data = DFmax, aes_string(x = "x", y = "y"), 
              color = "#CC0C00", position = position_nudge(y = 0.02)) +
    annotate(geom = "text", x = mean(DFmax$x), y = 1.6, label = "upper 25%",
             color = "#CC0C00") + 
    geom_point(aes_string(x = "x", y = "y"), data = DFwhisker, pch = "|", size = 10, 
               position = position_nudge(y=0.03), color = "#CC0C00") +
    geom_line(data = DFwhisker, aes_string(x = "x", y = "y"), 
              color = "#CC0C00", position = position_nudge(y = 0.02)) +
    annotate(geom = "text", x = mean(iqr), y = 1.85, 
             label = "maximum range of whiskers", color = "#CC0C00") +
    geom_line(data = DFout1, aes_string(x = "x", y = "y"), 
              color = "#CC0C00", position = position_nudge(y = 0.02)) +
    annotate(geom = "text", x = mean(DFout1$x), y = 1.85, 
             label = "outlier region", color = "#CC0C00") +
    geom_line(data = DFout2, aes_string(x = "x", y = "y"), 
              color = "#CC0C00", position = position_nudge(y = 0.02)) +
    annotate(geom = "text", x = mean(DFout2$x), y = 1.85, 
             label = "outlier region", color = "#CC0C00") +
    annotate(geom = "text", x = median(x), y = 0.35, label = "median", 
             angle = 270, color = "#5C88DA") +
    annotate(geom = "text", x = iqr[1], y = 0.35, 
             label = "1. quartile (Q1)", angle = 270, color = "#5C88DA") +
    annotate(geom = "text", x = iqr[2], y = 0.35, 
             label = "3. quartile (Q3)", angle = 270, color = "#5C88DA") +
    annotate(geom = "text", x = iqr[1]-1.5*diff(iqr), y = 0.35, 
             label = "1. quartile - 1.5 x IQR", angle = 270, vjust = 1.5,
             color = "#CC0C00") +
    annotate(geom = "text", x = low.fence, y = 0.35, 
             label = "lower fence", angle = 270, vjust = -0.75,
             color = "#5C88DA") +
    annotate(geom = "text", x = iqr[2]+1.5*diff(iqr), y = 0.35, 
             label = "3. quartile + 1.5 x IQR", angle = 270, vjust = -0.75,
             color = "#CC0C00") +
    annotate(geom = "text", x = upp.fence, y = 0.35, 
             label = "upper fence", angle = 270, vjust = 1.5,
             color = "#5C88DA") +
    annotate(geom = "text", x = (iqr[1]+iqr[2])/2, y = 0.05, 
             label = "IQR = Q3 - Q1", color = "#5C88DA") +
    ggtitle("Illustration of Box- and Whisker-Plot")
  print(gg)
  invisible(gg)
}
