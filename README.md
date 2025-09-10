# <img src="https://github.com/stamats/MKdescr/raw/master/hex-MKdescr.png" alt="MKdescr" width="120"/> &emsp; MKdescr
The repository includes the development version of R package MKdescr

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/MKdescr)](http://cran.r-project.org/package=MKdescr)
[![cran checks](https://badges.cranchecks.info/summary/MKdescr.svg)](https://cran.r-project.org/web/checks/check_results_MKdescr.html)

## Description
Computation of standardized interquartile range (IQR), Huber-type skipped mean 
(Hampel (1985), <doi:10.2307/1268758>), robust coefficient of variation (CV) 
(Arachchige et al. (2019), <doi:10.48550/arXiv.1907.01110>), robust signal to noise ratio (SNR), 
z-score, standardized mean difference (SMD), as well as functions that support 
graphical visualization such as boxplots based on quartiles (not hinges), negative 
logarithms and generalized logarithms for 'ggplot2' (Wickham (2016), ISBN:978-3-319-24277-4)

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
