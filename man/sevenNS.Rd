\name{sevenNS}
\alias{sevenNS}
\title{ Seven-Number Summaries }
\description{
  Function to compute seven-number summaries (minimum, 1st octile, 
  1st quartile, median, 3rd quartile, 7th octile, maximum)
}
\usage{
sevenNS(x, na.rm = TRUE, type = 7)
}
\arguments{
  \item{x}{ numeric vector }
  \item{na.rm}{ logical; remove \code{NA} before the computations. }
  \item{type}{ an integer between 1 and 9 selecting one of nine quantile
          algorithms; for more details see \code{\link[stats]{quantile}}. }
}
\details{
  In contrast to Tukey (1977) who proposes hinges and eighths, 
  we use function \code{\link[stats]{quantile}}.
}
\value{
  A numeric vector of length 7 containing the summary information.
}
\references{ Tukey, J.W. (1977). Exploratory Data Analysis. Section 2G. }
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{ \code{\link[stats]{fivenum}}, \code{\link[stats]{quantile}}  }
\examples{
x <- rnorm(100)
sevenNS(x)
sevenNS(x, type = 2)
}
\keyword{univar}
\keyword{distribution}
