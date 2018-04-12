\name{read.formind_data}
\alias{read.formind_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read.formind_data
}
\description{
  This function reads output of the FORMIND model.
}
\usage{
read.formind_data(path = "results/",name = "experiment", file_types="default", returnObejct = "Forest" )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
character string specifying the location of the result folder. Default is "Results/". Note string has to end with "/".
}
 \item{name}{
 name of the outputfiles.
 }
 \item{file_types}{
character string specifying the data type to load or the specific file or a string vector containing suffixes of files to load. Option "default" loads biomass, basal area, stem number and C-flux data if they are available. "hectare" loads all available data files which contain data aggregated on the hectare level. Other possible values are "patch" for plot level aggergated data; "cohort" for cohort level data; "patch_height" for data aggregated for every plot and height layer;"hectare_dbhClass" for data aggregated for every dbh class on the hectare level
}
 \item{returnObejct}{
 character string specifying the class of the output. The default is "Forest", which returns a Forest object. "data.table" return data.table
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
%%\examples{
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{  }
%%\keyword{  }% __ONLY ONE__ keyword per line
