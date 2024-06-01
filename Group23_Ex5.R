#.libPaths("D:/soft/r/4.1")#this row only for labs
library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
#library(readxl)
# library(knitr)
# library(rmarkdown)
library(simmer)
library(simmer.plot)


##----------------------------------------- 1.  all functions ------------------------------------------------

set.seed(456)

## add service
addService<- function  (trajectory,sname,timeDist){
  updatedPath <- seize(trajectory, resource = sname, amount = 1)%>%
    timeout(timeDist) %>%
    release(resource = sname, amount = 1)
  
  return(updatedPath)
}


#trimmed norm
trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}


#attributes for the family - number of kids, time at guard and time for mom
familyAtt <- function(){
  kids = rdiscrete(1, c(0.1,0.5,0.3,0.1), c(4,3,2,1))
    
  if (kids == 1){
    kidsNum = 1
    timeAtMallGuard = 3*trimmedNorm(1/3,1/6)
    timeForMom = runif(1,50,60)
  }
  
  if (kids == 2){
    kidsNum = 2
    timeAtMallGuard = 4*trimmedNorm(1/3,1/6)
    timeForMom = runif(1,80,100)
  }
  
  if (kids == 3){
    kidsNum = 3
    timeAtMallGuard = 5*trimmedNorm(1/3,1/6)
    timeForMom = runif(1,85,120)
  }
  
  if (kids == 4){
    kidsNum = 4
    timeAtMallGuard = 6*trimmedNorm(1/3,1/6)
    timeForMom = runif(1,100,150)
  }
  
  return (c(kidsNum,timeAtMallGuard,timeForMom))
}





#avgQueue
avgQueue <- function(time, queueLength, simTime){
  Lavg = 0;
  L = queueLength[1];
  Tnow = time[1];
  Llast = time[1];
  TL = 0;
  Tmax = simTime;
  if (length(time) == length(queueLength)){
    for (i in 2:length(time)){
      if(queueLength[i] != queueLength[i-1]){
        Tnow = time[i];
        TL = TL+L*(Tnow-Llast);
        L = queueLength[i];
        Llast = Tnow;
      }#if
    }#for
  }#end if
  TL=TL+L*(Tmax-Llast);
  Lavg = TL/Tmax;
  return (Lavg);
}#end func


##----------------------------------------- 2.  all simulation parameters ------------------------------------------------



simulationTime <- 12.5*60


##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mySimulation<- simmer("sim name")%>%
  add_resource("ParkingGuard",capacity=1,queue_size=Inf)%>%
  add_resource("MallGuard",capacity=2,queue_size=Inf)



##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------


## you can use mySimulation pointer when needed inside the trajectories. 

#path for impatient dads
angryDad <- trajectory("angryDad") %>%
  timeout(runif(1,60,70))%>%
  leave(1)
  

#dadPath <- trajectory("dadPath") %>%
 # branch(option=function() rdiscrete (1, c(0.67,0.33),c(0,1)) ,continue= c(FALSE),angryDad)%>%
  #synchronize(wait = T, mon_all = T)


#path for kids
kidsPath <- trajectory("kidsPath") %>%
  timeout(rtriangle(1,80,150,120))
  
#path for mom
momPath <- trajectory("momPath") %>%
  timeout(function() get_attribute(mySimulation,"timeForMom"))
  

#main path  
mall <- trajectory("mall") %>%
  addService("ParkingGuard", function () runif(1,1,2))%>%
  timeout(function() runif(1,2,8))%>%
  set_attribute(key=c("kidsNum","timeAtMallGuard","timeForMom"), value = function() familyAtt())%>%
  addService("MallGuard", function() get_attribute(mySimulation,"timeAtMallGuard"))%>%
  branch(option=function() rdiscrete (1, c(0.67,0.33),c(0,1)) ,continue= c(FALSE),angryDad)%>% #patient/ impatient dad path
  clone(n=2, momPath,  kidsPath)%>%
  synchronize(wait = T, mon_all = T)
  
  
  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
  
  
  mySimulation%>%
    add_generator("Family", mall, distribution = function() rexp(1,2), mon=2)

  
  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
  set.seed(456)
  reset(mySimulation)%>%run(until=simulationTime)
  
  plot(mall)
  
  arrivalData <- get_mon_arrivals(mySimulation)
  resourceData <- get_mon_resources(mySimulation)
  attributeData <- get_mon_attributes(mySimulation)
  
  
  
  
  #Q1 --------------------------------------------------------------------------
  ans <- sqldf("select name, max(end_time)-min(start_time) as totalTime
                from arrivalData
                group by name")
 
  ans <- mutate(ans, timeToPay = totalTime - 60)
  ans <- mutate(ans, quartersToPay = ceiling(timeToPay/15))
  ans <- mutate(ans, totalPrice = quartersToPay*7)
 
  
  ans1 <- sqldf("select sum(totalPrice)
                from ans
                where totalPrice > 0")
  
  paste("Total revenue from parking at the mall is: " ,ans1[1])
  
  
  
  
  #Q2 --------------------------------------------------------------------------
  
  parkingGuardData <- sqldf("select *
                              from resourceData
                            where resource=='ParkingGuard'")
  
  
  time <- as.matrix(parkingGuardData$time)
  queueLength <- as.matrix(parkingGuardData$queue);
  avgResQueue <- avgQueue(time, queueLength, simulationTime)
  paste("Average queue length for parking guard is ",avgResQueue, "families")
  
  
  max<- max(as.matrix(parkingGuardData$queue))
  paste("Max queue length for parking guard is ",max)
  
  
  if (avgResQueue > 15 || max > 15){
    paste("You need more parking guards")
  }
  if (avgResQueue <= 15 || max <= 15){    
    paste("You don't need more parking guards")
    
  }
  
   