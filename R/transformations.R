## adapted from scales:::log_breaks
glog_breaks <- function (n = 5, base = 10) {
  list(n, base)
  n_default <- n
  function(x, n = n_default) {
    raw_rng <- suppressWarnings(range(x, na.rm = TRUE))
    if (any(!is.finite(raw_rng))) {
      return(numeric())
    }
    rng <- glog(raw_rng, base = base)
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min) {
      return(inv.glog(min, base = base))
    }
    by <- floor((max - min)/n) + 1
    breaks <- inv.glog(seq(min, max, by = by), base = base)
    relevant_breaks <- inv.glog(rng[1], base = base) <= breaks & breaks <= inv.glog(rng[2], base = base)
    if (sum(relevant_breaks) >= (n - 2)) {
      return(breaks)
    }
    while (by > 1) {
      by <- by - 1
      breaks <- inv.glog(seq(min, max, by = by), base = base)
      relevant_breaks <- inv.glog(rng[1], base = base) <= breaks & breaks <= inv.glog(rng[2], base = base)
      if (sum(relevant_breaks) >= (n - 2)) {
        return(breaks)
      }
    }
    glog_sub_breaks(rng, n = n, base = base)
  }
}
## adapted from scales:::glog_sub_breaks
glog_sub_breaks <- function (rng, n = 5, base = 10){
  min <- floor(rng[1])
  max <- ceiling(rng[2])
  if (base <= 2) {
    return(inv.glog(min:max, base = base))
  }
  steps <- 1
  delta <- function(x) {
    min(diff(glog(sort(c(x, steps, base)), base = base)))
  }
  candidate <- seq_len(base)
  candidate <- candidate[1 < candidate & candidate < base]
  while (length(candidate)) {
    best <- which.max(vapply(candidate, delta, 0))
    steps <- c(steps, candidate[best])
    candidate <- candidate[-best]
    breaks <- as.vector(outer(inv.glog(seq(min, max), base = base), steps))
    relevant_breaks <- inv.glog(rng[1], base = base) <= breaks & breaks <= inv.glog(rng[2], base = base)
    if (sum(relevant_breaks) >= (n - 2)) {
      break
    }
  }
  if (sum(relevant_breaks) >= (n - 2)) {
    breaks <- sort(breaks)
    lower_end <- pmax(min(which(inv.glog(rng[1], base = base) <= breaks)) - 1, 1)
    upper_end <- pmin(max(which(breaks <= inv.glog(rng[2], base = base))) + 1, length(breaks))
    breaks[lower_end:upper_end]
  }
  else {
    extended_breaks(n = n)(inv.glog(rng, base = base))
  }
}
glog_trans <- function(base = exp(1)){
  trans_new("glog",
            transform = function(x) glog(x, base = base),
            inverse = function(x) inv.glog(x, base = base),
            breaks = glog_breaks(base = base),
            domain = c(-Inf, Inf))
}
glog10_trans <- function(){ glog_trans(base = 10) }
glog2_trans <- function(){ glog_trans(base = 2) }

scale_y_glog <- function(...){
  scale_y_continuous(..., trans = glog_trans())
}
scale_x_glog <- function(...){
  scale_x_continuous(..., trans = glog_trans())
}
scale_y_glog10 <- function(...){
  scale_y_continuous(..., trans = glog10_trans())
}
scale_x_glog10 <- function(...){
  scale_x_continuous(..., trans = glog10_trans())
}
scale_y_glog2 <- function(...){
  scale_y_continuous(..., trans = glog2_trans())
}
scale_x_glog2 <- function(...){
  scale_x_continuous(..., trans = glog2_trans())
}
scale_y_log <- function(...){
  scale_y_continuous(..., trans = log_trans())
}
scale_x_log <- function(...){
  scale_x_continuous(..., trans = log_trans())
}
scale_y_log2 <- function(...){
  scale_y_continuous(..., trans = log2_trans())
}
scale_x_log2 <- function(...){
  scale_x_continuous(..., trans = log2_trans())
}

neglog_breaks <- function (n = 5, base = 10){
  function(x){
    rng <- rev(-log(range(x, na.rm = TRUE), base = base))
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min)
      return(base^min)
    by <- floor((max - min)/n) + 1
    base^(seq(min, max, by = by))
  }
}
neglog_trans <- function(base = exp(1)){
  trans_new("neglog",
            transform = function(x) -log(x, base),
            inverse = function(x) base^(-x),
            breaks = neglog_breaks(base = base),
            domain = c(1e-100, Inf))
}
neglog10_trans <- function(){ neglog_trans(base = 10) }
neglog2_trans <- function(){ neglog_trans(base = 2) }
scale_y_neglog <- function(...){
  scale_y_continuous(..., trans = neglog_trans())
}
scale_x_neglog <- function(...){
  scale_x_continuous(..., trans = neglog_trans())
}
scale_y_neglog10 <- function(...){
  scale_y_continuous(..., trans = neglog10_trans())
}
scale_x_neglog10 <- function(...){
  scale_x_continuous(..., trans = neglog10_trans())
}
scale_y_neglog2 <- function(...){
  scale_y_continuous(..., trans = neglog2_trans())
}
scale_x_neglog2 <- function(...){
  scale_x_continuous(..., trans = neglog2_trans())
}
