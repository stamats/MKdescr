\name{zscore}
\alias{zscore}
\alias{medZscore}
\alias{iqrZscore}
\title{ Compute z-Scores }
\description{
  The functions compute the classical z-score as well as two robust versions of 
  z-scores.
}
\usage{
zscore(x, na.rm = FALSE)
medZscore(x, na.rm = FALSE, constant = 1/qnorm(0.75))
iqrZscore(x, na.rm = FALSE, type = 7, constant = 2*qnorm(0.75))
}
\arguments{
  \item{x}{ numeric vector. }
  \item{na.rm}{logical. Should missing values be removed?}
  \item{type}{ an integer between 1 and 9 selecting one of nine quantile
          algorithms; for more details see \code{\link[stats]{quantile}}. }
  \item{constant}{ standardizing contant; see \code{\link[stats]{mad}}
          and \code{\link{sIQR}}, respectively. }
}
\details{
  The functions compute the (classical) z-score as well as two robust variants.

  \code{medZscore} uses the (standardized) MAD instead of SD and median instead of mean.

  \code{iqrZscore} uses the (standardized) IQR instead of SD and median instead of mean.
}
\value{
  z-score.
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## 10% outliers
out <- rbinom(100, prob = 0.1, size = 1)
sum(out)
x <- (1-out)*rnorm(100, mean = 10, sd = 2) + out*25
z <- zscore(x)
z.med <- medZscore(x)
z.iqr <- iqrZscore(x)
## mean without outliers (should by close to 0)
mean(z[!out])
mean(z.med[!out])
mean(z.iqr[!out])
## sd without outliers (should by close to 1)
sd(z[!out])
sd(z.med[!out])
sd(z.iqr[!out])
}
\keyword{univar}
