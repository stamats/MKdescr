\name{illustrate.boxplot}
\alias{illustrate.boxplot}
\title{ Illustrate Box-and-Whisker Plots }
\description{
  Function to illustrate the computation of box-and-whisker plots.
}
\usage{
illustrate.boxplot(x)
}
\arguments{
  \item{x}{ numeric vector }
}
\details{
  The function visualizes the computation of box-and-whisker plots.
}
\value{
  An invisible object of class \code{ggplot}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{ \code{\link[graphics]{boxplot}}, \code{\link[ggplot2]{geom_boxplot}}  }
\examples{
set.seed(123)
illustrate.boxplot(rt(50, df = 5))
illustrate.boxplot(rnorm(50, mean = 3, sd = 2))
}
\keyword{univar}
\keyword{distribution}
