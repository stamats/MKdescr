\name{simCorVars}
\alias{simCorVars}
\title{ Simulate correlated variables. }
\description{
  The function simulates a pair of correlated variables.
}
\usage{
simCorVars(n, r, mu1 = 0, mu2 = 0, sd1 = 1, sd2 = 1, plot = TRUE)
}
\arguments{
  \item{n}{ integer: sample size. }
  \item{r}{ numeric: correlation.}
  \item{mu1}{ numeric: mean of first variable.}
  \item{mu2}{ numeric: mean of second variable.}
  \item{sd1}{ numeric: SD of first variable.}
  \item{sd2}{ numeric: SD of second variable.}
  \item{plot}{logical: generate scatter plot of the variables.}
}
\details{
  The function is mainly for teaching purposes and simulates \code{n} observations
  from a pair of normal distributed variables with correlation \code{r}.

  By specifying \code{plot = TRUE} a scatter plot of the data is generated.
}
\value{
  data.frame with entries \code{Var1} and \code{Var2}
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
set.seed(123)
res <- simCorVars(n = 100, r = 0.8)
cor(res$Var1, res$Var2)
colMeans(res)
apply(res, 2, sd)
set.seed(123)
res <- simCorVars(n = 100, r = 0.8, mu1 = -1, mu2 = 1, sd1 = 2, sd2 = 0.5)
cor(res$Var1, res$Var2)
colMeans(res)
apply(res, 2, sd)
}
\keyword{univar}
