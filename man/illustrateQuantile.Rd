\name{illustrate.quantile}
\alias{illustrate.quantile}
\title{ Illustrate Quantiles }
\description{
  Function to illustrate the computation of quantiles.
}
\usage{
illustrate.quantile(x, alpha, type)
}
\arguments{
  \item{x}{ numeric vector }
  \item{alpha}{ numeric value in the interval (0,1). }
  \item{type}{ integer values between 1 and 9 selecting one or several of 
               nine quantile algorithms; for more details see 
               \code{\link[stats]{quantile}}. If missing, all nine are 
               computed.}
}
\details{
  The function visualizes the computation of alpha-quantiles.
}
\value{
  An invisible object of class \code{ggplot}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{ \code{\link[stats]{quantile}}  }
\examples{
x <- 1:10
illustrate.quantile(x, alpha = 0.15)
illustrate.quantile(x, alpha = 0.5)
illustrate.quantile(x, alpha = 0.8, type = 2)
illustrate.quantile(x, alpha = 0.8, type = c(2, 7))

illustrate.quantile(x = rnorm(20), alpha = 0.95)
illustrate.quantile(x = rnorm(21), alpha = 0.95)
}
\keyword{univar}
\keyword{distribution}
