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
 
 
 addServiceRenge<- function  (trajectory,sname,timeDist,renegDist, continueTo){
   updatedPath <- renege_in(trajectory,renegDist,out= continueTo)%>%
     seize(resource = sname, amount = 1)%>%
     renege_abort()%>%
     timeout(timeDist) %>%
     release(resource = sname, amount = 1)
   
   return(updatedPath)
 }
 



##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

set.seed(456)
simulationTime <- 8*60
openTime<- rnorm(1,15,4)
door_schedule<-schedule(timetable = c(0, openTime), values = c(0, Inf), period = Inf)


##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mySimulation<- simmer("mySimulation")%>%
  add_resource("Door", capacity = door_schedule, queue_size = Inf)%>%
  add_resource("Tornado",capacity=1,queue_size=Inf)%>%
  add_resource("Tyfun",capacity=1,queue_size=Inf)%>%
  add_resource("Hurricane",capacity=1,queue_size=Inf)



##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

## you can use mySimulation pointer when needed inside the trajectories. 


Tornado<-trajectory("Tornado")%>%
  addService("Tornado",function() rtriangle(1,1/11,1/3,1/3))


Tyfun<-trajectory("Tyfun")%>%
  addServiceRenge ("Tyfun",function() rexp(1,1/3.5),function() runif(1,2,4),Tornado)


Hurricane<-trajectory("Hurricane")%>%
  addService ("Hurricane",function() rnorm(1,2.5,0.75))



kid<-trajectory("kid")%>%
  branch(option=function() rdiscrete (1, c(0.44,0.56),c(1,2)) ,continue= c(FALSE,FALSE),Tornado,Tyfun)


adult<-trajectory("adult")%>%
  branch(option=function() rdiscrete (1, c(0.47,0.53),c(1,2)) ,continue= c(FALSE,FALSE),Tornado,Hurricane)

customer<-trajectory("customer")%>%
  addService("Door",0)%>%
  timeout(function() runif(1,3,6.5))%>%
  branch(option=function() rdiscrete (1, c(0.6,0.4),c(1,2)) ,continue= c(FALSE,FALSE),kid,adult)

  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

mySimulation%>%
  add_generator("customer", customer, distribution = function () rexp(1,4.5), mon=2)


  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------

  reset(mySimulation)%>%run(until=simulationTime)
  
  plot(customer)

  
  arrivalData <- get_mon_arrivals(mySimulation)
  resourceData <- get_mon_resources(mySimulation)
  attributeData <- get_mon_attributes(mySimulation)
  
  
  
  # Question 1: ---------------------------------------------------------------------------------
  
  paste("The number of customers who waited by the door until the opening is:",
        sqldf("select MAX(queue)
            from resourceData
            where resource == 'Door' and capacity == 0 "))
  
  
  
  # Question 2: ---------------------------------------------------------------------------------
  
  arrivalData1 <- mutate(arrivalData,flowTime=end_time-start_time)
  paste(mean(arrivalData1$flowTime))
  
  
  
  # Question 3: ---------------------------------------------------------------------------------
  
  paste("The max queue for Tornado:",
        sqldf("select MAX(queue)
            from resourceData
            where resource = 'Tornado'"))
  
  
  paste("The max queue for Tyfun:",
        sqldf("select MAX(queue)
            from resourceData
            where resource = 'Tyfun'"))
  
  
  paste("The max queue for Hurricane:",
        sqldf("select MAX(queue)
            from resourceData
            where resource = 'Hurricane'"))

  