
dayrange <- function(act, act_se, speed_data){

  if(any(speed_data[,3] < 0 | (speed_data[,2]-speed_data[,1]) > 0 | act > 1 | act < 0)){
    stop("Wrong data. Use ?dayrange and check function description")
  }else{

    speed_data <- speed_data[order(speed_data[,1], decreasing = T), ]
    n_T <- 0
    for(i in 1:length(speed_data[,1])){ #Este loop es para calcular el "denominador"

      n_t <- speed_data[i,3]*(speed_data[1,1]/speed_data[i,1])
      n_T <- sum(n_T, n_t)

    }

    Dr <- 0; ses<-c()
    for(i in 1:length(speed_data[,1])){ #Este loop es para calcular el DR en m

      dr <- (speed_data[i,1]*act*(speed_data[1,1]/speed_data[i,1])*speed_data[i,3])/n_T
      Dr <- sum(Dr, dr)
      se <- dr*sqrt((act_se/act)^2+(speed_data[i,2]/speed_data[i,1])^2+((act_se/act)^2*(speed_data[i,2]/speed_data[i,1])^2))
      ses <- c(ses,se)

      if(i==length(speed_data[,1])){
        DR <<- Dr*86.4; SES<- 0
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
