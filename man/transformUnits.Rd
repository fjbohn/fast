\name{transformUnits}
\alias{transformUnits}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
transformation of Units 
}
\description{
transformation of a formind unit string of output files into a evaluatable string
}
\usage{
transformUnits(x,onlyCapitals=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of strings
}
  \item{onlyCapitals}{default=FALSE; if true only small letters at the beginning of a word are capitalized
vector of strings
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
unit<-c("Total Basal Area per PFT 2 [m2]")
unit<-transformUnits(unit)
plot(1,1,ylab=eval(as.formula(unit)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ }
%\keyword{ }% __ONLY ONE__ keyword per line
