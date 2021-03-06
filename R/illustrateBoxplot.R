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
  gg <- ggplot(DF, aes_string(x = "x", y = "y")) +
    theme_minimal() + 
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    geom_boxplot() + ylim(0, 2) + 
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
    annotate(geom = "text", x = mean(DFout1$x), y = 1.95, 
             label = "outlier region", color = "#CC0C00") +
    geom_line(data = DFout2, aes_string(x = "x", y = "y"), 
              color = "#CC0C00", position = position_nudge(y = 0.02)) +
    annotate(geom = "text", x = mean(DFout2$x), y = 1.95, 
             label = "outlier region", color = "#CC0C00") +
    annotate(geom = "text", x = median(x), y = 0.45, label = "median", 
             angle = 270, color = "#5C88DA") +
    annotate(geom = "text", x = iqr[1], y = 0.45, 
             label = "1. quartile", angle = 270, color = "#5C88DA") +
    annotate(geom = "text", x = iqr[2], y = 0.45, 
             label = "3. quartile", angle = 270, color = "#5C88DA") +
    annotate(geom = "text", x = iqr[1]-1.5*diff(iqr), y = 0.45, 
             label = "1. quartile - 1.5 x IQR", angle = 270, 
             color = "#5C88DA") +
    annotate(geom = "text", x = iqr[2]+1.5*diff(iqr), y = 0.45, 
             label = "3. quartile + 1.5 x IQR", angle = 270, 
             color = "#5C88DA") +
    ggtitle("Illustration of Box- and Whisker-Plot")
  print(gg)
  invisible(gg)
}
