\name{meanspeed}
\alias{meanspeed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate size-biased mean movement speed of a population sampled with camera-traps
}
\description{
When working with camera-trpas, the probability of sampling a given speed is proportional to itself. To estimate the average movement speed, a specific function should be applied to correct this size-biased sample. Different functions have been described (e.g. log-norma, gamma or Weibull), in this case we applied a Harmonic mean to estimate mean movement speed
}
\usage{
meanspeed(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The output of 'identbehv' function (i.e. a data frame containing in the first colum the speed values, and in the second column the behaviour group).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Mean and standard error of each movement state.
}
\references{
Palencia et al 2020 - in prep.
Rowcliffe, J. M., Jansen, P. A., Kays, R., Kranstauber, B., & Carbone, C. (2016). Wildlife speed cameras: measuring animal travel speed and day range using camera traps. Remote Sensing in Ecology and Conservation, 2(2), 84-94.
}
\author{
Pablo Palencia
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
behav_class <- data.frame(speed = c(runif(20, 0, 0.3), runif(30, 0.4, 0.7), runif(40, 0.8, 1.2)), behaviour = c(rep(1, 20), rep(2, 30), rep(3, 40)))
meanspeed(behav_class)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
