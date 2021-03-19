meanspeed <- function(x){
  f1 <- function(x){
    mean <- length(x)/sum(1/x)
    se <- mean^2 * sqrt(var(1/x)/length(x))
    c(mean, se)
  }
  res <- sapply(split(behav_class$speed, behav_class$behaviour), f1)
  ss <- as.data.frame(table(behav_class$behaviour))
  speed_data <<- data.frame(speeds=c(res[1,]),
                            speed_se=c(res[2,]),
                            n_seq=c(ss$Freq))

}
#
