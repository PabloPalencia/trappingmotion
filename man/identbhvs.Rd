\name{identbhvs}
\alias{identbhvs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Identify movement behaviours(states) in a population sampled with camera-traps.
}
\description{
Identify the optimal number of movement behaviours(states) by a k-means clustering. On trappingmotion version 2.0.0 I included a simplified version of 'identbhvs' released on version 1.0.0. Please, use 'identbhvs_mixdist' if you want to combine k-means clustering with log-normal mixture distributions when identifying movement behaviours.
Please, explore the presence of outliers in your data before running this function. You can use habitual procedures to identify outliers, such as visualization (boxplot), interquartile range and/or statistical tests (z scores). 'identify_outliers' function from 'rstatix' R package could be also useful.
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
Palencia, P., Fernandez-Lopez, J., Vicente, J., & Acevedo, P. (2021). Innovations in movement and behavioural ecology from camera traps: day range as model parameter. Methods in Ecology and Evolution, doi:10.1111/2041-210X.13609
}
\author{
Pablo Palencia
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
'meanspeed' function, 'identbhvs_mixdist' function
}
\examples{
dat2<-c(rnorm(80, 2, 0.8), rnorm(80, 0.2, 0.1), rnorm(30, 1, 0.1))
identbhvs(dat2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
