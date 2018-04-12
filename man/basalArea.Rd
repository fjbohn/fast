\name{basalArea}
\alias{basalArea}
\docType{data}
\title{
basal area data 
}
\description{
simulated data of an anden forest in Ecuador
}
\usage{data(basalArea)}
\format{
  A data frame with 502 observations on the following 7 variables.
  \describe{
    \item{\code{Time}}{a numeric vector}
    \item{\code{TotalBasalArea}}{a numeric vector}
    \item{\code{BasalAreaPerPFT_1}}{a numeric vector}
    \item{\code{BasalAreaPerPFT_2}}{a numeric vector}
    \item{\code{BasalAreaPerPFT_3}}{a numeric vector}
    \item{\code{BasalAreaPerComGRP_1}}{a numeric vector}
    \item{\code{BasalAreaPerComGRP_2}}{a numeric vector}
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
data(basalArea)
plot(basalArea$TotalBasalArea,type="l",lwd=2) 
points(basalArea$BasalAreaPerPFT_1,type="l",lwd=2,col="green")
points(basalArea$BasalAreaPerPFT_2,type="l",lwd=2,col="red")
points(basalArea$BasalAreaPerPFT_3,type="l",lwd=2,col="blue")
}
\keyword{datasets}
