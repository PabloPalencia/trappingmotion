identbhvs <- function(dat){
  muestra <-data.frame(dat*100); muestra <- subset(muestra, muestra >0); names(muestra) <- ("speed")

  if(max(muestra) > 600)message("Extreme speed values have been detected (higher than 20km/h -6m/s-). Please explore the presence of outliers, check the speed estimation on the pictures, and discard them if necessary")

  suppressWarnings(invisible(capture.output(res_NC <- NbClust(muestra, diss= NULL, distance = "euclidean", min.nc =2, max.nc=5, method = "kmeans"))))
  behav_class0 <- data.frame(cbind(muestra, res_NC$Best.partition))
  names(behav_class0) <- c("speed", "behaviour")
  behav_class0$speed <- behav_class0$speed/100
  behav_class <<- behav_class0

  plot_bhv <- ggplot(behav_class, aes(x=speed, color=as.factor(behaviour), fill=as.factor(behaviour))) +
    coord_cartesian(xlim = c(0, max(behav_class$speed)*1.2)) +
    geom_histogram(aes(y=after_stat(density)), alpha=0.5, position="identity", bins = 40) +
    scale_color_brewer(palette="Set1")+
    scale_fill_brewer(palette="Set1")+
    theme_classic()+
    xlab(bquote('Speed (m·s'^-1*')'))+
    ylab('Density')+
    geom_density(alpha=.2) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position="none")+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=20),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))

  plot_bhv

}
