meanspeed <- function(x){
  f1 <- function(x){
    mean <- length(x)/sum(1/x)
    se <- mean^2 * sqrt(var(1/x)/length(x))
    c(mean, se)
  }
  res <<- sapply(split(behav_class$speed, behav_class$behaviour), f1); res

}
#
