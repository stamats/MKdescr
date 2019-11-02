\name{skippedMean}
\alias{skippedMean}
\alias{skippedSD}
\title{Hyber-type Skipped Mean and SD}
\usage{
skippedMean(x, na.rm = FALSE, constant = 3.0)
skippedSD(x, na.rm = FALSE, constant = 3.0)
}
\description{Computes Huper-type Skipped Mean and SD.}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.rm}{logical. Should missing values be removed?}
  \item{constant}{ multiplier for outlier identification; see details below. }
}
\details{
  The Huber-type skipped mean and is very close to estimator X42 of Hampel 
  (1985), which uses 3.03 x MAD. Quoting Hampel et al. (1986), p. 69, the X42 
  estimator is "frequently quite reasonable, according to present preliminary 
  knowledge".
  
  For computing the Huber-type skipped mean, one first computes median and
  MAD. In the next step, all observations outside the interval 
  [median - 3 x MAD, median + 3 x MAD] are removed and arithmetic mean and 
  sample standard deviation are computed on the remaining data.
}
\references{
  Hampel, F.R. (1985). The breakdown points of the mean combined with some 
  rejection rules. \emph{Technometrics}, \bold{27}: 95-107.
  
  Hampel, F.R., Ronchetti, E.M., Rousseeuw, P.J., Stahel, W.A (1986). 
  \emph{Robust statistics. The approach based on influence functions.} 
  New York: Wiley.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{
 \code{\link[base]{mean}}, \code{\link[stats]{sd}}, \code{\link[stats]{median}}, 
 \code{\link[stats]{mad}}.
}
\examples{
## normal data
x <- rnorm(100)
mean(x)
median(x)
skippedMean(x)

sd(x)
mad(x)
skippedSD(x)

## Tukey's gross error model
## (1-eps)*Norm(mean, sd = sigma) + eps*Norm(mean, sd = 3*sigma)
ind <- rbinom(100, size = 1, prob = 0.1)
x.err <- (1-ind)*x + ind*rnorm(100, sd = 3)
mean(x.err)
median(x.err)
skippedMean(x.err)

sd(x.err)
mad(x.err)
skippedSD(x.err)
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}

