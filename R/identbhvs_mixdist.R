identbhvs_mixdist <- function(dat){
  muestra <-data.frame(dat*100); muestra <- subset(muestra, muestra >0); names(muestra) <- ("speed")

  if(max(muestra) > 600)message("Extreme speed values have been detected (higher than 20km/h -6m/s-). Please explore the presence of outliers, check the speed estimation on the pictures, and discard them if necessary")

  suppressWarnings(invisible(capture.output(res_NC <- NbClust(muestra, diss= NULL, distance = "euclidean", min.nc =2, max.nc=5, method = "kmeans"))))
  resul <- data.frame(cbind(muestra, res_NC$Best.partition)); names(resul) <- c("speed", "KGroup")

  n_c <- length(unique(res_NC$Best.partition))
  res_m <- aggregate(speed ~ KGroup, resul, "mean")
  res_m <- res_m[order(res_m$speed),]
  res_sd <- aggregate( speed ~ KGroup, resul, sd)
  res_sd <- res_sd[order(res_sd$speed),]
  res_n <- aggregate( speed ~ KGroup, resul, length)
  res_m[1,3] <- res_m[2,2]/res_m[1,2]; res_m[2,3] <- res_m[3,2]/res_m[2,2]; res_m[3,3] <- res_m[4,2]/res_m[3,2]; res_m[4,3] <- res_m[5,2]/res_m[4,2];
  res_m[1,4] <- res_n[1,2]; res_m[2,4] <- res_n[2,2]; res_m[3,4] <- res_n[3,2]; res_m[4,4] <- res_n[4,2]; res_m[5,4] <- res_n[5,2]
  res_m <- subset(res_m, V4 > 15)
  if (max(na.omit(res_m$V3)) < 2 | length(res_m[,1]) <= 1){
    behav_class1 <- data.frame(dat); names(behav_class1) <- ("speed")
    behav_class1$behaviour <- 1
    behav_class <<- behav_class1
    return(cat("One movement behaviour have been identified"))

  }else{
    res_m$V3[is.na(res_m$V3)] <- 5; res_m <- na.omit(res_m); res_m <- subset(res_m, V3 > 2)

    hh <- hist(muestra[,1], breaks= round(((max(muestra[,1])-min(muestra[,1]))*10), 1), plot = FALSE, warn.unused = FALSE) # length interval=0.1
    dd <- as.data.frame(cbind(hh$breaks,hh$counts[+1]))

    ddpar2 <- na.omit(as.data.frame(cbind(c(res_n[1,2]/sum(res_n[,2]), res_n[2,2]/sum(res_n[,2]), res_n[3,2]/sum(res_n[,2]), res_n[4,2]/sum(res_n[,2]), res_n[5,2]/sum(res_n[,2])), c(res_m[1,2], res_m[2,2], res_m[3,2], res_m[4,2], res_m[5,2]), c(res_sd[1,2], res_sd[2,2], res_sd[3,2], res_sd[4,2], res_sd[5,2])))) # reference values

    fitpike2 <- mix(dd, ddpar2, "lnorm", constr = mixconstr(consigma = "CCV"), emsteps = 20)

    cc <- data.frame(fitted(fitpike2))
    ee<-cbind(cc, dd)

    muestra$speed_r <-round((muestra$speed), 1)

    ee$V1 <-round(((trunc(ee$V1*100)/100)*10/10),1)

    colnames(ee)[which(names(ee) == "V1")] <- "speed_r"
    jj<-merge(muestra, ee, by= "speed_r", all.x = T)
    names(jj)[2]<-"speed"


    jj <- na.omit(jj)
    for(k in 1:length(jj[,1])){
      if(length(fitpike2$parameters[,1]) == 2){
        zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[1]
        zz <- as.numeric(sub("conditprob.", "", names(zz)))
        jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz

        if(jj[k,length(jj[1,])] == 0){

          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[2]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- zz
        }
      }

      if(length(fitpike2$parameters[,1]) == 3){
        zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[1]
        zz <- as.numeric(sub("conditprob.", "", names(zz)))
        jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz

        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[2]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz
        }
        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[3]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- zz
        }
      }

      if(length(fitpike2$parameters[,1]) == 4){
        zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[1]
        zz <- as.numeric(sub("conditprob.", "", names(zz)))
        jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz

        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[2]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz
        }
        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[3]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz
        }
        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[4]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- zz
        }
      }

      if(length(fitpike2$parameters[,1]) == 5){
        zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[1]
        zz <- as.numeric(sub("conditprob.", "", names(zz)))
        jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz

        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[2]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz
        }
        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[3]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz
        }
        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[4]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- rbinom(1, 1, (max(jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)])))*zz
        }
        if(jj[k,length(jj[1,])] == 0){
          zz <- arrange((jj[k,((length(jj[1,])-length(fitpike2$parameters[,1]))):(length(jj[1,])-1)]), decreasing = T)[5]
          zz <- as.numeric(sub("conditprob.", "", names(zz)))
          jj[k,length(jj[1,])] <- zz
        }
      }

    }


    names(jj)[length(jj[1,])] <- "behaviour"
    jj$speed <- jj$speed/100
    behav_class <<- jj %>% select(speed, behaviour)

    plot_bhv <- ggplot(behav_class, aes(x=speed, color=as.factor(behaviour), fill=as.factor(behaviour))) +

      coord_cartesian(xlim = c(0, max(behav_class$speed)*1.2))+
      geom_histogram(aes(y=..density..), alpha=0.5,
                     position="identity") +
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
            panel.border = element_rect(colour = "black", fill=NA, size=1))
    plot_bhv
  }

}
