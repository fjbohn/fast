\name{biomass}
\alias{biomass}
\docType{data}
\title{
biomass data 
}
\description{
simulated data of an anden forest in Ecuador
}
\usage{data(biomass)}
\format{
  A data frame with 502 observations on the following 7 variables.
  \describe{
    \item{\code{Time}}{a numeric vector}
    \item{\code{TotalBiomass}}{a numeric vector}
    \item{\code{BiomassPerPFT_1}}{a numeric vector}
    \item{\code{BiomassPerPFT_2}}{a numeric vector}
    \item{\code{BiomassPerPFT_3}}{a numeric vector}
    \item{\code{BiomassPerComGRP_1}}{a numeric vector}
    \item{\code{BiomassPerComGRP_2}}{a numeric vector}
  }
}
%%\details{

%%}
%%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%%}
%%\references{
%%  ~~ possibly secondary sources and usages ~~
%%}
\examples{
data(biomass)
plot(biomass$TotalBiomass,type="l",lwd=2) 
points(biomass$BiomassPerPFT_1,type="l",lwd=2,col="green")
points(biomass$BiomassPerPFT_2,type="l",lwd=2,col="red")
points(biomass$BiomassPerPFT_3,type="l",lwd=2,col="blue")
}
\keyword{datasets}
