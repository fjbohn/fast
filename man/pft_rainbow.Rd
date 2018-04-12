\name{pft_rainbow}
\alias{pft_rainbow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
pft_rainbow
}
\description{
 generate a collor palette based on the HCL and HSV color spaces.   
 }
\usage{
pft_rainbow(n,alpha=NA,lum.fix=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
the number of colors (>= 1) to be in the palette.
}
  \item{alpha}{
the alpha transparency, a number in [0,1]
}
 \item{lum.fix}{
logical. If TRUE all collors have the same luminicens. If FALSE, all collors vary in luminicens and hence in grayscale if printed in black and white.
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
## convenience demo functions
pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
## a few useful diverging HCL palettes
par(mar = rep(0, 4), mfrow = c(4, 1))
pal(pft_rainbow(7))
pal(pft_rainbow(7,alpha=0.5))
pal(pft_rainbow(7,lum.fix=TRUE))
pal(pft_rainbow(7,alpha=0.5,lum.fix=TRUE))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{  }
%%\keyword{  }% __ONLY ONE__ keyword per line
