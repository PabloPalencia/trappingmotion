\name{identbhvs}
\alias{identbhvs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify movement behaviours(states) in a population sampled with camera-traps.
}
\description{
Identify movement behaviours(states) by a clustering and mixture distribution process. A k-means clustering scheme has been applied to estimate the optimal number of movement behaviours, and log-normal distributions have been considered to classify the speed values.
}
\usage{
identbhvs(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
A numeric vector of speed data in m/s
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame in which the first column includes the speeds, and the second column includes the movement behaviour in which each speed has been classified. A density plot with the speed clustering is obtained by default.
}
\references{
Palencia et al. 2020 - in prep
}
\author{
Pablo Palencia
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
'meanspeed' function
}
\examples{
dat2<-c(rnorm(80, 2, 0.8), rnorm(80, 0.2, 0.1), rnorm(30, 1, 0.1))
identbhvs(dat2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
