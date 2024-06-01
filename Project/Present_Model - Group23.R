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

#setting the initial attributes
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
  numOfMachines=0
  tiredness=0
  ifDone=0
  isNutrition=0
  isPhysyo=0
  return (c(video, count, numOfMachines, tiredness,ifDone,isNutrition,isPhysyo))
}


#counting the number of videos that need to watch in a row
trainingAtt<-function(){
  count = get_attribute(mySimulation,"count")+1
  return (count)
}

#counting the number of machines that the gymnast has done
machinesAtt<-function(){
  numOfMachines = get_attribute(mySimulation,"numOfMachines")+1
  return (numOfMachines)
}

#checks if the woman can keep training - if not too tired or hasn't done all her machines
canWomenKeepTraining<-function(){
  if (get_attribute(mySimulation,"numOfMachines")< 4 && get_attribute(mySimulation,"tiredness")<=2.4){
    return (TRUE)
  }
  else{
    return (FALSE)
    
  }
}

#checks if the man can keep training - if not too tired or hasn't done all his machines
canMenKeepTraining<-function(){
  if (get_attribute(mySimulation,"numOfMachines")< 6 && get_attribute(mySimulation,"tiredness")<=2.9){
    return (TRUE)
  }
  else{
    return (FALSE)
    
  }
}




#returns the accomulative tiredness
getTiredness<-function(){
  u<-runif(1,0,1)
  if (u<=0.5){
    return ((3*u/8)^(1/3)+get_attribute(mySimulation, "tiredness"))
  }
  if (u> 0.5 && u<= 2/3){
    return (((u+2/3)/2)+get_attribute(mySimulation, "tiredness"))
  }
  if (u>2/3){
    return (((-6+sqrt(12*(1-u)))/-6)+get_attribute(mySimulation, "tiredness"))
  }
}



#setting priority according to tiredness
prioritySetWomen<- function(){
  if(get_attribute(mySimulation,"tiredness") >2.4){
    v<-c(1,1,F)
   return(v)
  }
  else{
    v<-c(0,0,F)
    return(v)
  }
}

#setting priority according to tiredness
prioritySetMen<- function(){
  if(get_attribute(mySimulation,"tiredness") >2.9){
    v<-c(1,1,F)
    return(v)
  }
  else{
    v<-c(0,0,F)
    return(v)
  }
}


#returns the machines that the woman gymnast hasn't done yet
resourceToVisitWomen<-function(){
  v<-c()
  if(get_attribute(mySimulation,"ifGround") == 0){
    v<-append(v,1)
  }
  if(get_attribute(mySimulation,"ifSteppedParallels") == 0){
    v<-append(v,2)
  }
  if(get_attribute(mySimulation,"ifBeam1") == 0 && get_attribute(mySimulation,"ifBeam2") == 0){
    p<-runif(1,0,1)
    if(p<0.5){
      v<-append(v,3)
    }
    if(p>=0.5){
      v<-append(v,4)
    }
  }
  if(get_attribute(mySimulation,"ifJumps1") == 0){
    v<-append(v,5)
  }
  
  return(sample(v,1))
}


#returns the machines that the man gymnast hasn't done yet
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
  if(get_attribute(mySimulation,"ifJumps2") == 0){
    v<-append(v,6)
  }
  
  return(sample(v,1))
  
}

breakTime<-function (){
  p<-trimmedNorm(6,50/60) 
  return (p+420)
}



##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

#set.seed(456)
simulationTime <- 14*60
door_schedule<-schedule(timetable = c(0, 120), values = c(0, Inf), period = Inf)
#breakTime<- trimmedNorm(6,50/60) 

#setting the capacity and queue sizes according to schedule
break_schedule<-schedule(timetable = c(0,420,breakTime()), values = c(2,0,2), period = Inf)
nutrition_schedule<-schedule(timetable = c(0,120,420,breakTime()), values = c(0,1,0,1), period = Inf)
PY_break_schedule<-schedule(timetable = c(0,120,360,420,breakTime(),600), values = c(0,2,5,0,5,3), period = Inf)
video_schedule<-schedule(timetable = c(0,120,420,breakTime()), values = c(0,2,0,2), period = Inf)
video_queue<-schedule(timetable = c(0,120), values = c(0,Inf), period = Inf)
manager_schedule<-schedule(timetable = c(0,300,360), values = c(0,Inf,0), period = Inf)

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mySimulation<- simmer("mySimulation")%>%
  add_resource("Door", capacity = door_schedule, queue_size = 0)%>%
  add_resource("womenDressingRoom",capacity=20,queue_size=Inf)%>%
  add_resource("menDressingRoom",capacity=20,queue_size=Inf)%>%
  add_resource("womenShower",capacity=5,queue_size=Inf)%>%
  add_resource("menShower",capacity=5,queue_size=Inf)%>%
  add_resource("video1",capacity=video_schedule,queue_size=video_queue)%>%
  add_resource("video2",capacity=video_schedule,queue_size=video_queue)%>%
  add_resource("video3",capacity=video_schedule,queue_size=video_queue)%>%
  add_resource("video4",capacity=video_schedule,queue_size=video_queue)%>%
  add_resource("video5",capacity=video_schedule,queue_size=video_queue)%>%
  add_resource("parallels",capacity=1,queue_size=Inf)%>%
  add_resource("rings",capacity=1,queue_size=Inf)%>%
  add_resource("tension",capacity=1,queue_size=Inf)%>%
  add_resource("adjacentHorse",capacity=1,queue_size=Inf)%>%
  add_resource("ground",capacity=1,queue_size=Inf)%>%
  add_resource("jumps1",capacity=,queue_size=Inf)%>%
  add_resource("jumps2",capacity=1,queue_size=Inf)%>%
  add_resource("steppedParallels",capacity=1,queue_size=Inf)%>%
  add_resource("beam1",capacity=1,queue_size=Inf)%>%
  add_resource("beam2",capacity=1,queue_size=Inf)%>%
  add_resource("nutrition1",capacity=nutrition_schedule,queue_size=Inf)%>%
  add_resource("nutrition2",capacity=nutrition_schedule,queue_size=Inf)%>%
  add_resource("physiotherapist",capacity=PY_break_schedule,queue_size=Inf, preemptive = FALSE)%>%
  add_resource("manager", capacity=manager_schedule, queue_size=manager_schedule )



##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------


## you can use mySimulation pointer when needed inside the trajectories.

#------------------------- Machines Trajs --------------------------------------
#for every machine: set that has been done, count to watch vide, count number of
#machines that has been done and get tiredness.

managerTRJ<-trajectory("managerTRJ")%>%
  seize("manager",1)%>%
  set_attribute(key=c("tiredness"), value=0)%>%
  release("manager",1)

ground<-trajectory("ground")%>%
  addService ("ground",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifGround"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

steppedParallels<-trajectory("steppedParallels")%>%
  addService ("steppedParallels",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifSteppedParallels"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

beam1<-trajectory("beam1")%>%
  addService ("beam1",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifBeam1"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

beam2<-trajectory("beam2")%>%
  addService ("beam2",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifBeam2"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

jumps1<-trajectory("jumps1")%>%
  addService ("jumps1",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifJumps1"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

jumps2<-trajectory("jumps2")%>%
  addService ("jumps2",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifJumps2"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

parallels<-trajectory("parallels")%>%
  addService ("parallels",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifParallels"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

rings<-trajectory("rings")%>%
  addService ("rings",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifRings"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

tension<-trajectory("tension")%>%
  addService ("tension",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifTension"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())

adjacentHorse<-trajectory("adjacentHorse")%>%
  addService ("adjacentHorse",function() trimmedNorm(5,1.7))%>%
  set_attribute(key=c("ifAdjacentHorse"),value=(1))%>%
  set_attribute(key=c("count"),value=function() trainingAtt())%>%
  set_attribute(key=c("tiredness"), value= function() getTiredness())%>%
  set_attribute(key=c("numOfMachines"),value=function() machinesAtt())
#-------------------------------------------------------------------------------


#----------- Times And Breakes Trajs -------------------------------------------


#setting the door queue size to be inf after 8:00 am
door<- trajectory("door") %>%
  set_queue_size("Door",Inf)


#setting the next nutrition lecture to be every round hour
batchScheduleTrj<- trajectory("batchScheduleTrj")%>%
  set_global(key="timeForBatch",value=(function () now(mySimulation)+60))


#-------------------------------------------------------------------------------

#nutrition - watching lecture in groups of 10 or in a round hour
nutritionTrajWomen<- trajectory("nutritionTrajWomen")%>%
  batch(n=10,timeout=function() {get_global(mySimulation,"timeForBatch")-now(mySimulation)},permanent=FALSE)%>%
  addService("nutrition1", function() runif(1,30,40))%>%
  separate()%>%
  set_attribute(key=c("isNutrition"),value=1)

#nutrition - watching lecture in groups of 10 or in a round hour
nutritionTrajMen<- trajectory("nutritionTrajMen")%>%
  batch(n=10,timeout=function() {get_global(mySimulation,"timeForBatch")-now(mySimulation)},permanent=FALSE)%>%
  addService("nutrition2", function() runif(1,30,40))%>%
  separate()%>%
  set_attribute(key=c("isNutrition"),value=1)

#physiotherapy traj
physiotherapyTraj<- trajectory("physiotherapyTraj")%>%
  addService("physiotherapist",rtriangle(1,25,40,33))%>%
  set_attribute(key=c("isPhysyo"),value=1)
  



#traj for training - branching to machines
womenTraining<-trajectory("womenTraining")%>%
  branch(option=function() resourceToVisitWomen() ,continue= c(TRUE,TRUE,TRUE,TRUE,TRUE),ground,steppedParallels,beam1,beam2, jumps1)%>%              #go training
  #join(managerTRJ)%>%
  set_prioritization(function() prioritySetWomen())


#traj for training - branching to machines
menTraining<-trajectory("menTraining")%>%
  branch(option= function() resourceToVisitMen() ,continue= c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE),parallels,rings,tension,adjacentHorse,ground, jumps2)%>%
  #join(managerTRJ)%>%
  set_prioritization(function() prioritySetMen())
  



#traj for women who succeeded entering the video rooms - keep training and watching videos.
#after done training/ too tired - going to video again and then nutrition & physio/ physio only and to shower.videoTrajWomen<-trajectory("videoTrajWomen")%>%
videoTrajWomen<-trajectory("videoTrajWomen")%>%
  timeout(0)%>%
  release("Door",1)%>%
  simmer::select(resource= function()
  paste0("video", get_attribute(mySimulation, "video")))%>%     #seize video 
  seize_selected(amount=1)%>%
  timeout(function() trimmedNorm(3,45/60)*(get_attribute(mySimulation, "count")))%>%  #watch video
  release_selected(amount=1)%>%
  set_attribute(key=c("count"),value=0)%>%              #updating no more video duplication to watch
  join(womenTraining)%>%
  rollback(amount=7, times=100, check=function() canWomenKeepTraining())%>%    #go training again----
  branch(option=function() rdiscrete (1, c(0.32,0.68),c(0,1)) ,continue= c(TRUE),nutritionTrajWomen)%>%
  join(physiotherapyTraj)%>%
  addService("womenShower",function() runif(1,8,14))#%>%
  #log_("dlsjfhdfhsohfos")



#traj for men who succeeded entering the video rooms - keep training and watching videos.
#after done training/ too tired - going to video again and then nutrition & physio/ physio only and to shower.
videoTrajMen<-trajectory("videoTrajMen")%>%
  timeout(0)%>%
  release("Door",1)%>%
  simmer::select(resource= function()
  paste0("video", get_attribute(mySimulation, "video")))%>%     #seize video  
  seize_selected(amount=1)%>%
  timeout(function() trimmedNorm(3,45/60)*(get_attribute(mySimulation, "count")))%>%  #watch video
  release_selected(amount=1)%>%
  set_attribute(key=c("count"),value=0)%>%              #updating no more video duplication to watch
  join(menTraining)%>%
  rollback(amount=7, times=100, check=function() canMenKeepTraining())%>%    #go training again----
  simmer::select(resource= function()
  paste0("video", get_attribute(mySimulation, "video")))%>%     #seize video  
  seize_selected(amount=1)%>%
  timeout(function() trimmedNorm(3,45/60)*(get_attribute(mySimulation, "count")))%>%  #watch video
  release_selected(amount=1)%>%
  set_attribute(key=c("count"),value=0)%>%              #updating no more video duplication to watch
  branch(option=function() rdiscrete (1, c(0.32,0.68),c(0,1)) ,continue= c(TRUE),nutritionTrajMen)%>%
  join(physiotherapyTraj)%>%
  addService("menShower",function() runif(1,8,14))



#main traj for women - going to dressing rooms and then training. then trying to watch video.
#women who watched video - moving to video traj and stays there.
#women who can't watch video (didn't arrive yet), going to train again and return to this traj.
#after done training/ too tired - going to video again and then nutrition & physio/ physio only and to shower.
womenTraj<- trajectory("womenTraj")%>%
  set_attribute(key=c("video","count","numOfMachines","tiredness","ifDone","isPhysyo","isNutrition"),value=function() gymnasticAtt())%>%
  addService("womenDressingRoom",function() runif(1,3,5))%>%    #dressing room
  set_attribute(key=c("ifGround","ifSteppedParallels","ifBeam1","ifBeam2","ifJumps1"),value=c(0,0,0,0,0))%>%
  join(womenTraining)%>%
  seize("Door",amount = 1, continue= c(F,T), post.seize=videoTrajWomen,reject=womenTraining)%>%             #try to enter the video room
  rollback(amount=2, times=100, check=function() canWomenKeepTraining())%>%    #go training again----
  simmer::select(resource= function()
  paste0("video", get_attribute(mySimulation, "video")))%>%     #seize video  
  seize_selected(amount=1)%>%
  timeout(function() trimmedNorm(3,45/60)*(get_attribute(mySimulation, "count")))%>%  #watch video
  release_selected(amount=1)%>%
  set_attribute(key=c("count"),value=0)%>%              #updating no more video duplication to watch
  branch(option=function() rdiscrete (1, c(0.32,0.68),c(0,1)) ,continue= c(TRUE),nutritionTrajWomen)%>%
  join(physiotherapyTraj)%>%
  addService("womenShower",function() runif(1,8,14))#%>%
  #log_("dlsjfhdfhsohfos")


#main traj for men - going to dressing rooms and then training. then trying to watch video.
#men who watched video - moving to video traj and stays there.
#men who can't watch video (didn't arrive yet), going to train again and return to this traj.
#after done training/ too tired - going to video again and then nutrition & physio/ physio only and to shower.
menTraj <- trajectory("menTraj") %>%
  set_attribute(key=c("video","count","numOfMachines","tiredness","ifDone","isPhysyo","isNutrition"),value=function() gymnasticAtt())%>%
  addService("menDressingRoom",function() runif(1,3,5))%>%
  set_attribute(key=c("ifParallels","ifRings","ifTension","ifAdjacentHorse","ifGround","ifJumps2"),value=c(0,0,0,0,0,0))%>%
  join(menTraining)%>%
  seize("Door",amount = 1, continue= c(F,T), post.seize =videoTrajMen, reject=menTraining)%>%             #try to enter the video room
  rollback(amount=2, times=100, check=function() canMenKeepTraining())%>%   #for those who came before the video guys - go training again
  simmer::select(resource= function()
  paste0("video", get_attribute(mySimulation, "video")))%>%     #seize video  
  seize_selected(amount=1)%>%
  timeout(function() trimmedNorm(3,45/60)*(get_attribute(mySimulation, "count")))%>%  #watch video
  release_selected(amount=1)%>%
  set_attribute(key=c("count"),value=0)%>%              #updating no more video duplication to watch
  branch(option=function() rdiscrete (1, c(0.32,0.68),c(0,1)) ,continue= c(TRUE),nutritionTrajMen)%>%
  join(physiotherapyTraj)%>%
  addService("menShower",function() runif(1,8,14))






##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

mySimulation%>%
  add_generator("MenGymnast", menTraj, distribution = to(540,function () rexp(1,0.8815592)), priority=0, mon=2)%>%
  add_generator("WomanGymnast", womenTraj, distribution = to(540,function () rexp(1,0.7744995)), priority=0, mon=2)%>%
  add_generator("batchSchedule",batchScheduleTrj,from (0,function ()(60)), mon=2)%>%
  add_generator("doorQueue",door,distribution=at(120), mon=2)
  



##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
#set.seed(456)
#reset(mySimulation)%>%run(until=simulationTime)


arrivalData <- get_mon_arrivals(mySimulation, ongoing=F)
resourceData <- get_mon_resources(mySimulation)
attributeData <- get_mon_attributes(mySimulation)



a<-(sqldf("select count(*)
            from attributeData as A join attributeData as ARR on A.name=ARR.name
            where A.name like 'MenGymnast%' and A.key='numOfMachines'and A.value='6' and ARR.key='isPhysyo' and ARR.value='1'"))

b<-(sqldf("select count(*)
            from attributeData as A join attributeData as ARR on A.name=ARR.name
            where A.name like 'WomanGymnast%' and A.key='numOfMachines'and A.value='4' and ARR.key='isPhysyo' and ARR.value='1'"))


c<-(sqldf("select count(*)
            from arrivalData
            where name like 'MenGymnast%' or name like 'WomanGymnast%'"))

paste("Precent num of gymnast done training:",(a+b)/c)



d<-(sqldf("select end_time-start_time as [flow]
            from arrivalData 
            where name like 'menGymnast%' or name like 'womanGymnast%'"))

print(mean(d$flow))
print(sd(d$flow))


for (i in 1:15){
  amenSheyaavod[i]=(a.count[i]+b.count[i])/n.count[i]
}



#--------------
mm2envs <- mclapply(1:15, function(i) {
  set.seed(456+i)
  reset(mySimulation)%>%run(until=840)%>%
  wrap()
})


fullData2<-get_mon_arrivals(mm2envs) # the full data of all replications
attributeData2<- get_mon_attributes(mm2envs) # the full data of all replications





a<-(sqldf("select A.replication, count(*)
            from attributeData2 as A join attributeData2 as ARR on A.name=ARR.name
            where A.name like 'MenGymnast%' and A.key='numOfMachines'and A.value='6' and ARR.key='isPhysyo' and ARR.value='1'
            group by A.replication"))

b<-(sqldf("select A.replication, count(*)
            from attributeData2 as A join attributeData2 as ARR on A.name=ARR.name
            where A.name like 'WomanGymnast%' and A.key='numOfMachines'and A.value='4' and ARR.key='isPhysyo' and ARR.value='1'
            group by A.replication"))


c<-(sqldf("select replication, count(*)
            from fullData2
            where name like 'MenGymnast%' or name like 'WomanGymnast%'
            group by replication"))






#Average flow time for every run
FlowMeanData2 <- sqldf("select replication, avg(end_time - start_time) as meanFlow
                        from fullData2
                        where name like 'menGymnast%' or name like 'womanGymnast%'
                        group by replication")
  
  
#Average flow time for every run
DonePercentageMeanData2 <- sqldf("select replication, 


