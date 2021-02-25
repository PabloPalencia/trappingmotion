dayrange <- function(act, act_se, speed_data){

  if(any(speed_data[,3] < 0 | (speed_data[,2]-speed_data[,1]) > 0 | act > 1 | act < 0)){
    stop("Wrong data. Use ?dayrange and check function description")
  }else{

    speed_data <- speed_data[order(speed_data[,1], decreasing = T), ]

    for(i in 1:length(speed_data[,1])){

      speed_data[i,4] <- speed_data[i,3]*(speed_data[1,1]/speed_data[i,1])

    }; names(speed_data)[4] <- "m"

    for(i in 1:length(speed_data[,1])){

      speed_data[i,5] <- speed_data[i,4]/sum(speed_data[,4])

    }; names(speed_data)[5] <- "p"


    Dr <- c(); ses<-c()
    for(i in 1:length(speed_data[,1])){

      sepi <- sqrt((speed_data[i,5]*(1-speed_data[i,5]))/(sum(speed_data[,4])-1))
      ai <- act*speed_data[i,5]
      seai <- ai*sqrt((sepi/speed_data[i,5])^2+(act_se/act)^2+((act_se/act)^2*(sepi/speed_data[i,5])^2))
      dri <- speed_data[i,1]*ai
      sedri <- dri*sqrt((speed_data[i,2]/speed_data[i,1])^2+(seai/ai)^2+((speed_data[i,2]/speed_data[i,1])^2*(seai/ai)^2))

      Dr <- sum(Dr, dri)
      ses <- c(ses, sedri)

      if(i==length(speed_data[,1])){
        DR <<- Dr*86.4; SES<- c()
        for(i in 1:length(speed_data[,1])){

          SE<- ses[i]^2
          SES <- c(SES, SE)
        }

        DR_se <<- sqrt(sum(SES))*86.4
      }
    }
    return(cat("Day range (Km/day)", DR,
               "\nDay range SE (Km/day)", DR_se))


  }

}
