\name{zfactor}
\alias{zfactor}
\alias{medZfactor}
\alias{iqrZfactor}
\title{ Compute z-Factors }
\description{
  The functions compute the z-factor as well as two robust versions of 
  z-factors.
}
\usage{
zfactor(x, y, na.rm = FALSE)
medZfactor(x, y, na.rm = FALSE, constant = 1/qnorm(0.75))
iqrZfactor(x, y, na.rm = FALSE, type = 7, constant = 2*qnorm(0.75))
}
\arguments{
  \item{x}{ numeric vector (with usually positive numbers), results of sample or positive control. }
  \item{y}{ numeric vector (with usually positive numbers), results of (negative) control. }
  \item{na.rm}{logical. Should missing values be removed?}
  \item{type}{ an integer between 1 and 9 selecting one of nine quantile
          algorithms; for more details see \code{\link[stats]{quantile}}. }
  \item{constant}{ standardizing contant; see \code{\link[stats]{mad}}
          and \code{\link{sIQR}}, respectively. }
}
\details{
  The functions compute the z-factor as well as two robust variants. The z-factor
  is a measure of effect size used in high-throughput screenings. It is used to 
  decide whether the signal in a specific experiment is large enough to warrant 
  further investigation. Zhang et al. (1999) introduce the following categorisation:
  \itemize{
    \item z-factor = 1: ideal assay
    \item 1 > z-factor >= 0.5: excellent assay
    \item 0.5 > z-factor > 0: doable assay
    \item z-factor = 0: "yes/no" type assay
    \item z-factor < 0: screening essentially impossible
  }

  \code{medZfactor} uses the (standardized) MAD instead of SD and median instead of mean.

  \code{iqrZfactor} uses the (standardized) IQR instead of SD and median instead of mean.
  
  Since the definition of z-factor and z'-factor coincide, the functions can also 
  be used to compute z'-factors; see formulas (4) and (5) in Zhang et al. (1999).
}
\value{
  z-factor.
}
\references{
  J.H. Zhang, T.D.Y. Chung and K.R. Oldenburg (1999). A simple statistical 
  parameter for use in evaluation and validation of high throughput screening assays. 
  \emph{Journal of Biomolecular Screening} \bold{4} (2): 67-73.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
x <- rnorm(100, mean = 10, sd = 2)
y <- rnorm(100, mean = 40, sd = 2)
zfactor(x, y)
medZfactor(x, y)
iqrZfactor(x, y)
}
\keyword{univar}
