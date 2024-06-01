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

addService<- function  (path,sname,attName){
  updatedPath <- seize(path, sname)%>%
    timeout_from_attribute(attName) %>%
    release(sname)
  
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

#function to set the student's attributes
studentsAtt<-function(i){
  # i=1: year1, i=2: year2, i=3: year3, i=4: year4
  if (i==1){
    sYear = 1
    Lab48Time = runif(1,30,60)
    Lab42Time = trimmedNorm(30,60)
    Lab41Time = rtriangle(1,30,60,45)
    Lab40Time = 180
  }
  if (i==2){
    sYear = 2
    Lab48Time = runif(1,30,120)
    Lab42Time = trimmedNorm(30,120)
    Lab41Time = rtriangle (1,30,120,75)
    Lab40Time = 180
  }
  if (i==3){
    sYear = 3
    Lab48Time = runif(1,60,240)
    Lab42Time = trimmedNorm(60,240)
    Lab41Time = rtriangle (1,60,240,150)
    Lab40Time = 180
  }
  if (i==4){
    sYear = 4
    Lab48Time = runif(1,60,120)
    Lab42Time = trimmedNorm(60,120)
    Lab41Time = rtriangle (1,60,120,90)
    Lab40Time = 180
  }
  return (c(sYear,Lab48Time,Lab42Time,Lab41Time,Lab40Time))
}


##----------------------------------------- 2.  all simulation parameters ------------------------------------------------


simulationTime <- 16*60


##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mySimulation<- simmer("mySimulation")%>%
  add_resource("Lab40",capacity=20,queue_size=Inf)%>%
  add_resource("Lab41",capacity=20,queue_size=Inf)%>%
  add_resource("Lab42",capacity=20,queue_size=Inf)%>%
  add_resource("Lab48",capacity=20,queue_size=Inf)


##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------


## you can use mySimulation pointer when needed inside the trajectories. 

#year A leaving after not finding available computer
yearAFinish<-trajectory("yearAFinish")%>%
  leave(1)

#year B after not finding available computer
yearBFinish<-trajectory("yearBFinish")%>%
  log_("I yearBFinish ")%>%
  timeout(function () rtriangle(1,8,20,11))%>%
  set_attribute(key = "triedAgain",value = 1)%>%
  addServiceRenge("Lab40","Lab40Time",1, NULL)%>%
  set_attribute(key = "Succeed",value = 1)


#year C after not finding available computer
yearCFinish<-trajectory("yearCFinish")%>%
  log_("I yearCFinish ")%>%
  timeout(function () runif(1,10,16))%>%
  set_attribute(key = "triedAgain",value = 1)%>%
  addServiceRenge("Lab40","Lab40Time",1, NULL)%>%
  set_attribute(key = "Succeed",value = 1)


#year D trying again to catch computer in lab 40 after not finding available computer
yearDTryAgain<-trajectory("yearDTryAgain")%>%
  log_("I yearDTryAgain ")%>%
  set_attribute(key = "triedAgainD",value = 1)%>%
  addServiceRenge("Lab40","Lab40Time",1, NULL)%>%
  set_attribute(key = "Succeed",value = 1)


#year D after not finding available computer - try again or go home
yearDFinish<-trajectory("yearDFinish")%>%
  log_("I yearDFinish ")%>%
  timeout(function () rexp(1,1/16))%>%
  branch(option=function() rdiscrete (1, c(0.75,0.25),c(0,1)) ,continue= c(FALSE),yearDTryAgain)%>%
  leave(1)


#All students after not finding available computer in lab 40
angryStudents<-trajectory("angryStudents")%>%
  branch(option=function() get_attribute(mySimulation,"sYear") ,continue= c(FALSE,FALSE,FALSE,FALSE),yearAFinish,yearBFinish,yearCFinish,yearDFinish)


#trajectory for lab 40
Traj40<-trajectory("Traj40")%>%
  addServiceRenge ("Lab40","Lab40Time",1,angryStudents)


#trajectory for lab 41
Traj41<-trajectory("Traj41")%>%
  addServiceRenge ("Lab41","Lab41Time",1,Traj40)


#trajectory for lab 42
Traj42<-trajectory("Traj42")%>%
  addServiceRenge ("Lab42","Lab42Time",1,Traj41)


#trajectory for lab 48
Traj48<-trajectory("Traj48")%>%
  addServiceRenge ("Lab48","Lab48Time",1,Traj42)


#trajectory for students at year A
StudentsA<-trajectory("StudentsA")%>%
  set_attribute(key=c("sYear","Lab48Time","Lab42Time","Lab41Time","Lab40Time"),value=function() studentsAtt(1))%>%
  branch(option=function() rdiscrete (1, c(0,0.15,0.35,0.41,0.09),c(0,1,2,3,4)) ,continue= c(FALSE,FALSE,FALSE,FALSE),Traj48, Traj42, Traj41, Traj40)


#trajectory for students at year B
StudentsB<-trajectory("StudentsB")%>%
  set_attribute(key=c("sYear","Lab48Time","Lab42Time","Lab41Time","Lab40Time"),value=function() studentsAtt(2))%>%
  branch(option=function() rdiscrete (1, c(0,0.15,0.35,0.41,0.09),c(0,1,2,3,4)) ,continue= c(FALSE,FALSE,FALSE,FALSE),Traj48, Traj42, Traj41, Traj40)


#trajectory for students at year C
StudentsC<-trajectory("StudentsC")%>%
  set_attribute(key=c("sYear","Lab48Time","Lab42Time","Lab41Time","Lab40Time"),value=function() studentsAtt(3))%>%
  branch(option=function() rdiscrete (1, c(0,0.15,0.35,0.41,0.09),c(0,1,2,3,4)) ,continue= c(FALSE,FALSE,FALSE,FALSE),Traj48, Traj42, Traj41, Traj40)


#trajectory for students at year D
StudentsD<-trajectory("StudentsD")%>%
  set_attribute(key=c("sYear","Lab48Time","Lab42Time","Lab41Time","Lab40Time"),value=function() studentsAtt(4))%>%
  branch(option=function() rdiscrete (1, c(0,0.15,0.35,0.41,0.09),c(0,1,2,3,4)) ,continue= c(FALSE,FALSE,FALSE,FALSE),Traj48, Traj42, Traj41, Traj40)




##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------


mySimulation%>%
  add_generator("StudentA", StudentsA, to(9*60, function () rexp(1,1/20)), mon=2)%>%
  add_generator("StudentB", StudentsB, to(9*60, function () rexp(1,1/7)), mon=2)%>%
  add_generator("StudentC", StudentsC, to(9*60, function () rexp(1,1/3)), mon=2)%>%
  add_generator("StudentD", StudentsD, to(9*60, function () rexp(1,1/10)), mon=2)



##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
set.seed(456)
reset(mySimulation)%>%run(until=simulationTime)


arrivalData <- get_mon_arrivals(mySimulation)
arrivalData2 <- get_mon_arrivals(mySimulation, per_resource=TRUE)
resourceData <- get_mon_resources(mySimulation)
attributeData <- get_mon_attributes(mySimulation)



#Q1

tryAgainD <- sqldf("select count (*) from attributeData where key == 'triedAgainD'")
paste("Number of students at year D who tried again catching computer: " ,tryAgainD)


#Q2
studentsNum <- sqldf("select count(*) from arrivalData")
noComputers <- sqldf("select count(*) from arrivalData where finished == false")
p <- noComputers/studentsNum
paste("Precentage of students with no computer: " ,p)


