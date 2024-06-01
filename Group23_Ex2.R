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



##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime <- 7*60


##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------

mySimulation<- simmer("mySimulation")%>%
  add_resource("Lawyer",capacity=1,queue_size=Inf)%>%
  add_resource("Representitive",capacity=2,queue_size=Inf)



##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

## you can use mySimulation pointer when needed inside the trajectories. 
  student<-trajectory("student")%>%
    seize("Lawyer",1)%>%
    timeout(function() rnorm(1,5,1.5))%>%
    release("Lawyer",1)%>%
  
    seize("Representitive",1)%>%
    timeout(function() runif(1,3,4.5))%>%
    release("Representitive",1)
  
  ##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------
  
  mySimulation%>%
    add_generator("student", student, distribution = function () rexp(1,1.5), mon=2)

  
  
  ##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------
  
  set.seed(123)
  reset(mySimulation)%>%run(until=simulationTime)
  
  
  
  # Question 1: -------------------------------------
  
  plot(student)
  
  
  # Question 2: -------------------------------------
  
  arrivalDataF <- get_mon_arrivals(mySimulation, ongoing = F, per_resource = T) #all finished arrivals, per resource 
  resourceData <- get_mon_resources(mySimulation)

  allArrivalData <- get_mon_arrivals(mySimulation)
  finished <- sum(allArrivalData$finished == TRUE)
  print(finished)
  
  
  # Question 3: -------------------------------------
  
  arrivalData1 <- allArrivalData%>%
    mutate(flow_time = end_time - start_time)
  print(mean(arrivalData1$flow_time))
  

  # Question 4: -------------------------------------
  
  arrivalDataFinished <- get_mon_arrivals(mySimulation, ongoing = F)  #all finished arrivals, not per resource 
  notFinished <- sum(allArrivalData$finished) - sum(arrivalDataFinished$finished)
  print(notFinished)
  
  
  # Question 5: -------------------------------------

  maxRepresentitiveQueue <- max(resourceData$queue[resourceData$resource == "Representitive"])
  print(maxRepresentitiveQueue)
  
  
  
  