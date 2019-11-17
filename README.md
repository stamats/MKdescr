# MKdescr
The repository includes the development version of R package MKdescr

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/MKdescr)](http://cran.r-project.org/package=MKdescr)
[![cran checks](https://cranchecks.info/badges/summary/MKdescr)](https://cran.r-project.org/web/checks/check_results_MKdescr.html)

## Description
Functions that are not available in standard R packages 
such as standardized interquartile range (IQR), Huber-type skipped mean, 
robust coefficient of variation (CV), robust signal to noise ratio (SNR), 
as well as functions that support graphical visualization such as boxplots 
based on quartiles (not hinges), negative logarithms and generalized logarithms 
for ggplots.

## Installation

```{r, eval = FALSE}
## Installation of CRAN version
install.packages("MKdescr")

## Development version from GitHub
# install.packages("remotes")
remotes::install_github("stamats/MKdescr")
```

## Getting started

```{r}
library(MKdescr)
```
