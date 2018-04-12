\name{treeNumber}
\alias{treeNumber}
\docType{data}
\title{
tree number data 
}
\description{
simulated data of an anden forest in Ecuador
}
\usage{data(treeNumber)}
\format{
  A data frame with 502 observations on the following 7 variables.
  \describe{
    \item{\code{Time}}{a numeric vector}
    \item{\code{TotalNumber}}{a numeric vector}
    \item{\code{NumberPerPFT_1}}{a numeric vector}
    \item{\code{NumberPerPFT_2}}{a numeric vector}
    \item{\code{NumberPerPFT_3}}{a numeric vector}
    \item{\code{NumberPerComGRP_1}}{a numeric vector}
    \item{\code{NumberPerComGRP_2}}{a numeric vector}
  }
}
%%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%%}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
data(treeNumber)
plot(treeNumber$TotalNumber,type="l",lwd=2) 
points(treeNumber$NumberPerPFT_1,type="l",lwd=2,col="green")
points(treeNumber$NumberPerPFT_2,type="l",lwd=2,col="red")
points(treeNumber$NumberPerPFT_3,type="l",lwd=2,col="blue")
}
\keyword{datasets}
