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
## add service
addService<- function  (trajectory,sname,timeDist){
  updatedPath <- seize(trajectory, resource = sname, amount = 1)%>%
    timeout(timeDist) %>%
    release(resource = sname, amount = 1)
  
  return(updatedPath)
}


#add service with reneging
addServiceRenge<- function  (trajectory,sname,attName,renegDist, continueTo){
  updatedPath <- renege_in(trajectory,renegDist,out= continueTo)%>%
    seize(resource = sname, amount = 1)%>%
    renege_abort()%>%
    timeout_from_attribute(attName) %>%
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


gymnasticAtt<-function(){
  p<-runif(1,0,1)
  
  if (p<0.2){
    video=1
  }
  if (p>=0.2 && p< 0.4){
    video=2
  }
  if (p>=0.4 && p< 0.6){
    video=3
  }
  if (p>=0.6 && p< 0.8){
    video=4
  }
  if (p>=0.8){
    video=5
  }
  count=0
  return (c(video, count))
}


trainingAtt<-function(){
  count = get_attribute(mySimulation,"count")+1
  return (count)
}


resourceToVisitWomen<-function(){
  v<-c()
  if(get_attribute(mySimulation,"ifGround") == 0){
    v<-append(v,1)
  }
  if(get_attribute(mySimulation,"ifSteppedParallels") == 0){
    v<-append(v,2)
  }
  if(get_attribute(mySimulation,"ifBeam") == 0){
    v<-append(v,3)
  }
  if(get_attribute(mySimulation,"ifJumps") == 0){
    v<-append(v,4)
  }
  
  return (v)
}


#parallels,rings,tension,adjacentHorse,ground, jumps
resourceToVisitMen<-function(){
  v<-c()
  if(get_attribute(mySimulation,"ifParallels") == 0){
    v<-append(v,1)
  }
  if(get_attribute(mySimulation,"ifRings") == 0){
    v<-append(v,2)
  }
  if(get_attribute(mySimulation,"ifTension") == 0){
    v<-append(v,3)
  }
  if(get_attribute(mySimulation,"ifAdjacentHorse") == 0){
    v<-append(v,4)
  }
  if(get_attribute(mySimulation,"ifGround") == 0){
    v<-append(v,5)
  }
  if(get_attribute(mySimulation,"ifJumps") == 0){
    v<-append(v,6)
  }
  
  return (v)
}


stationsVec<-function(){
  v<-c()
  s<-function() get_attribute(mySimulation,"stations")
  for (val in 1:s){
    v<-append(v,as.double(1/s))
  }
  v<-as.double(v)
  return (v)
}





f1<-function(){
  
  v1<-function() stationsVec
  v2<-function() resourceToVisitWomen
  r<-rdiscrete(1,v1,v2)
return(r)
}


##----------------------------------------- 2.  all simulation parameters ------------------------------------------------



simulationTime <- 14*60
door_schedule<-schedule(timetable = c(0, 2*60), values = c(0, 10), period = Inf)



##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mySimulation<- simmer("mySimulation")%>%
  add_resource("Door", capacity = door_schedule, queue_size = Inf)%>%
  add_resource("womenDressingRoom",capacity=20,queue_size=Inf)%>%
  add_resource("menDressingRoom",capacity=20,queue_size=Inf)%>%
  add_resource("womenShower",capacity=5,queue_size=Inf)%>%
  add_resource("menShower",capacity=5,queue_size=Inf)%>%
  add_resource("video1",capacity=2,queue_size=Inf)%>%
  add_resource("video2",capacity=2,queue_size=Inf)%>%
  add_resource("video3",capacity=2,queue_size=Inf)%>%
  add_resource("video4",capacity=2,queue_size=Inf)%>%
  add_resource("video5",capacity=2,queue_size=Inf)%>%
  add_resource("parallels",capacity=1,queue_size=Inf)%>%
  add_resource("rings",capacity=1,queue_size=Inf)%>%
  add_resource("tension",capacity=1,queue_size=Inf)%>%
  add_resource("adjacentHorse",capacity=1,queue_size=Inf)%>%
  add_resource("ground",capacity=1,queue_size=Inf)%>%
  add_resource("jumps",capacity=2,queue_size=Inf)%>%
  add_resource("steppedParallels",capacity=1,queue_size=Inf)%>%
  add_resource("beam",capacity=2,queue_size=Inf)
  


##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------


## you can use mySimulation pointer when needed inside the trajectories. 

ground<-trajectory("ground")%>%
  addService ("ground",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifGround"),value=(1))

steppedParallels<-trajectory("steppedParallels")%>%
  addService ("steppedParallels",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifSteppedParallels"),value=(1))


beam<-trajectory("beam")%>%
  addService ("beam",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifBeam"),value=(1))


jumps<-trajectory("jumps")%>%
  addService ("jumps",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifJumps"),value=(1))


parallels<-trajectory("parallels")%>%
  addService ("parallels",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifParallels"),value=(1))


rings<-trajectory("rings")%>%
  addService ("rings",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifRings"),value=(1))


tension<-trajectory("tension")%>%
  addService ("tension",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifTension"),value=(1))


adjacentHorse<-trajectory("adjacentHorse")%>%
  addService ("adjacentHorse",function() trimmedNoem(5,1.7))%>%
  set_attribute(key=c("ifAdjacentHorse"),value=(1))




#--------------------------------------------------------------
  
womenTraj<- trajectory("womenTraj")%>%
  set_attribute(key=c("video","count"),value=function() gymnasticAtt())%>%
  addService("womenDressingRoom",function() runif(1,3,5))%>%
  set_attribute(key=c("ifGround","ifSteppedParallels","ifBeam","ifJump","stations"),value=c(0,0,0,0,4))%>%
  branch(option=function() rdiscrete (1,function() stationsVec, function() resourceToVisitWomen) ,continue= c(TRUE,TRUE,TRUE,TRUE),ground,steppedParallels,beam,jumps)%>%
  set_attribute(key=c("stations"),value=function() get_attribute(mySimulation,"stations")-1)%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  rollback(amount=3, times=3)  

  
  menTraj <- trajectory("menTraj") %>%
    set_attribute(key=c("video","count"),value=function() gymnasticAtt())%>%
    addService("menDressingRoom",function() runif(1,3,5))%>%
    set_attribute(key=c("ifParallels","ifRings","ifTension","ifAdjacentHorse","ifGround","ifJumps","stations"),value=c(0,0,0,0,0,0,6))%>%
    branch(option=function() rdiscrete (1,function() stationsVec, function() resourceToVisitMen) ,continue= c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),parallels,rings,tension,adjacentHorse,ground, jumps)%>%
    set_attribute(key=c("stations"),value=function() get_attribute(mySimulation,"stations")-1)%>%
    set_attribute(key=c("count"),value=function() trainingAtt())%>%
    rollback(amount=3, times=5)
  

  
  
  
    
  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
  
  
  mySimulation%>%
    add_generator("MenGymnast", menTraj, distribution = function () rexp(1,0.8815592), mon=2)%>%
    add_generator("WomenGymnast", womenTraj, distribution = function () rexp(1,0.7744995), mon=2)
  
  
  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
  set.seed(456)
  reset(mySimulation)%>%run(until=simulationTime)
  
  plot(menTraj)

  arrivalData <- get_mon_arrivals(mySimulation)
  resourceData <- get_mon_resources(mySimulation)
  attributeData <- get_mon_attributes(mySimulation)
  