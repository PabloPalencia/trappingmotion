\name{dayrange}
\alias{dayrange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate day range (daily distance travelled)
}
\description{
Estimates the average daily distance travlled by each individual in the population sampled with camera-traps. For that, it is needed activity and speed
}
\usage{
dayrange(act, act_se, speed_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{act}{
Number of activity rate of the population. Takes values between 0 and 1. See 'fitact' in 'activity' R package for details.
}
  \item{act_se}{
Number of activity rate standard error.
}
  \item{speed_data}{
A data frame containing 3 columns and i rows (the output of 'meanspeed' function). Each row includes parameters for each movement behaviour (state) identified. First column: average movement speed expresed in m/s; second column: standard error of average movement speed; third column: number of animals classified on each behaviour.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Day range expresed in km/day
}
\references{
Palencia, P., Fernandez-Lopez, J., Vicente, J., & Acevedo, P. (2021). Innovations in movement and behavioural ecology from camera traps: day range as model parameter. Methods in Ecology and Evolution, doi:10.1111/2041-210X.13609
\nRowcliffe, J. M., Kays, R., Kranstauber, B., Carbone, C., & Jansen, P. A. (2014). Quantifying levels of animal activity using camera trap data. Methods in Ecology and Evolution, 5(11), 1170-1179.
}
\author{
Pablo Palencia
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
'identbhvs' and 'meanspeed' functions
}
\examples{
speed_data <-data.frame(speeds=c(0.6, 0.9, 0.01), speeds_se=c(0.1, 0.2, 0.001), n_seq=c(50, 30, 10))
dayrange(act =0.68, act_se = 0.05, speed_data = speed_data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
