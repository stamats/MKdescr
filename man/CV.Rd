\name{CV}
\alias{CV}
\alias{medCV}
\alias{iqrCV}
\title{ Compute CV }
\description{
  The functions compute coefficient of variation (CV) as well as two robust 
  versions of the CV.
}
\usage{
CV(x, na.rm = FALSE)
medCV(x, na.rm = FALSE, constant = 1/qnorm(0.75))
iqrCV(x, na.rm = FALSE, type = 7, constant = 2*qnorm(0.75))
}
\arguments{
  \item{x}{ numeric vector with positive numbers. }
  \item{na.rm}{logical. Should missing values be removed?}
  \item{type}{ an integer between 1 and 9 selecting one of nine quantile
          algorithms; for more details see \code{\link[stats]{quantile}}. }
  \item{constant}{ standardizing contant; see \code{\link[stats]{mad}}
          and \code{\link{sIQR}}, respectively. }
}
\details{
  The functions compute the (classical) CV as well as two robust variants.

  \code{medCV} uses the (standardized) MAD instead of SD and median instead of mean.

  \code{iqrCV} uses the (standardized) IQR instead of SD and median instead of mean.
}
\value{
  CV value.
}
\references{
  C.N.P.G. Arachchige, L.A. Prendergast and R.G. Staudte. Robust analogues
  to the Coefficient of Variation. https://arxiv.org/abs/1907.01110.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## 5% outliers
out <- rbinom(100, prob = 0.05, size = 1)
sum(out)
x <- (1-out)*rnorm(100, mean = 10, sd = 2) + out*25
CV(x)
medCV(x)
iqrCV(x)
}
\keyword{univar}
