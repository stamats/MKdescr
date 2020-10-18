\name{SMD}
\alias{SMD}
\title{ Compute Standardized Mean Difference (SMD)}
\description{
  The function computes the standardized mean difference, where a bias correction
  can be applied.
}
\usage{
SMD(x, y, bias.cor = TRUE, var.equal = FALSE, na.rm = FALSE)
}
\arguments{
  \item{x}{ numeric vector, data of group 1. }
  \item{y}{ numeric vector, data of group 2. }
  \item{bias.cor}{a logical variable indicating whether a bias correction should
          be performed.}
  \item{var.equal}{a logical variable indicating whether to treat the two variances 
          as being equal. If \code{TRUE} then the pooled variance is used to estimate 
          the variance otherwise the Welch-Satterthwaite approximation is used.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\details{
  The function compute the (bias-corrected) standardized mean difference. 
  
  If \code{bias.cor = FALSE} and \code{var.equal = TRUE}, the result corresponds 
  to Cohen's d (Cohen (1988)).

  If \code{bias.cor = TRUE} and \code{var.equal = TRUE}, the result corresponds to 
  Hedges' g (Hedges (1981)).
  
  If \code{bias.cor = FALSE} and \code{var.equal = FALSE}, the result is closely
  related to the test statistic of Welch's t test (Aoki (2020)).

  If \code{bias.cor = TRUE} and \code{var.equal = FALSE}, the result corresponds to 
  Aoki's e (Aoki (2020)) which incorporates a Welch-Satterthwaite approximation 
  in combination with a bias correction.
}
\value{
  SMD value.
}
\references{
  Aoki, S. (2020). Effect sizes of the differences between means without assuming 
  variance equality and between a mean and a constant. \emph{Heliyon}, \bold{6}(1), e03306.
  
  Cohen, J. (1988). Statistical Power Analysis for the Behavioral Sciences. 
  Routledge. ISBN 978-1-134-74270-7.
  
  Hedges, L. V. (1981). Distribution theory for Glass's estimator of effectsize 
  and related estimators. \emph{Journal of Educational Statistics} \bold{6}, 107-128.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
n1 <- 200
x <- rnorm(n1)
n2 <- 300
y <- rnorm(n2, mean = 3, sd = 2)
## true value
(0-3)/sqrt((1 + n1/n2*2^2)/(n1/n2+1))
## estimates
## Aoki's e
SMD(x, y)
## Hedges' g
SMD(x, y, var.equal = TRUE)
## standardized test statistic of Welch's t-test
SMD(x, y, bias.cor = FALSE)
## Cohen's d
SMD(x, y, bias.cor = FALSE, var.equal = TRUE)

## Example from Aoki (2020)
SMD(0:4, c(0, 0, 1, 2, 2))
SMD(0:4, c(0, 0, 1, 2, 2), var.equal = TRUE)
SMD(0:4, c(0, 0, 1, 2, 2), bias.cor = FALSE)
SMD(0:4, c(0, 0, 1, 2, 2), bias.cor = FALSE, var.equal = TRUE)
}
\keyword{univar}
