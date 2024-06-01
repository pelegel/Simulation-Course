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


filePath=choose.files() 
table<-read.csv(filePath,header=FALSE) #show view from Global Envitroment. No editing. Press blue arrow for data types (Factor levels)
table<-table[complete.cases(table),] 


#---------1-A-------------
rows <- nrow(table)
cols <- ncol(table)

# paste = the function concatenate strings
paste("num of rows" , rows , ", num of col", cols)



#---------1-B-------------
last4col1 <- table[8:11,1]
mean1 <- mean(last4col1)
print(mean1)

last4col2 <- table[8:11,2]
mean2 <- mean(last4col2)
print(mean2)

last4col3 <- table[8:11,3]
mean3 <- mean(last4col3)
print(mean3)

last4col4 <- table[8:11,4]
mean4 <- mean(last4col4)
print(mean4)

last4col5 <- table[8:11,5]
mean5 <- mean(last4col5)
print(mean5)

last4col6 <- table[8:11,6]
mean6 <- mean(last4col6)
print(mean6)



#---------1-C-------------
row1 <- table[1,]
sd1 <- sd(row1)

row2 <- table[2,]
sd2 <- sd(row2)

row3 <- table[3,]
sd3 <- sd(row3)

row4 <- table[4,]
sd4 <- sd(row4)

row5 <- table[5,]
sd5 <- sd(row5)

row6 <- table[6,]
sd6 <- sd(row6)

row7 <- table[7,]
sd7 <- sd(row7)

row8 <- table[8,]
sd8 <- sd(row8)

row9 <- table[9,]
sd9 <- sd(row9)

row10 <- table[10,]
sd10 <- sd(row10)

row11 <- table[11,]
sd11 <- sd(row11)



#---------1-D-------------
#the function "which" returns the indexes when arr.ind is selected as TRUE. 
which(table == max(table), arr.ind = TRUE)



#---------1-E-------------
vecevenF <- function(table){
  vecsol<-0
  l=1
  for (i in 1:11){
    for (j in 1:6){
      
      if (table[i,j]%%2==0){
        vecsol[l]<- table[i,j]
        l=l+1
      }
    }
  }
  return(vecsol)
}
sol<-vecevenF(table)
print(sol)



#---------1-F-------------
sol <- matrix(nrow = 11, ncol = 6)
for (row in 1:nrow(table)) {
  for (col in 1:ncol(table)) {
    sol[row, col] = sqrt(table[row, col])
  }
}



#---------1-G-------------
col5 <- table[,5]
vec <- vector("numeric",11)
sum = 0

for (row in 1:11) {
  sum = sum + col5[row]
  vec[row] = sum
}

print(vec)




#---------1-H-------------
normvec <- vector("numeric",8)
normvec <-rnorm(n=8,mean=3,sd=2) 




#---------1-I-------------
s <- sd(normvec) 
alfa <- 0.05
mu <- mean(normvec) 
n <- length(normvec) 
tval <- qt(1-(alfa/2),df=n-1) 
delta <- tval*s/sqrt(n)

CI <- c(mu-delta,mu+delta) 
print(CI)



#---------1-J-------------
mat <- matrix(nrow = 50, ncol = 100)
for (i in 1:50){
  for (j in 1:100){
    mat[i,j] = runif(1,0,1)
  }
}

hist(mat)



#---------2----------------
winners <- vector("numeric",5)
for (i in 1:100){

    # first step - sample a propability and check in which interval it falls. add 1 to the relevant interval's counter.
    # second step - sample another propability and choose the second winner in equal propabilities
    p1 <- runif(1,0,1)
    
    
    if (p1 <= 0.31){
      winners[1] = winners[1] + 1
      
      p2 <- runif(1,0,1)
      
      if(p2 <= 0.25){
        winners[2] = winners[2] + 1
      }
      if(p2 > 0.25 && p2 <= 0.5){
        winners[3] = winners[3] + 1
      }
      if(p2 > 0.5 && p2 <= 0.75){
        winners[4] = winners[4] + 1
      }
      if (p2 > 0.75 && p2 <= 1){
        winners[5] = winners[5] + 1
      }
    }
    
    
    if (p1 > 0.31 && p1 <= 0.6){
      winners[2] = winners[2] + 1
      
      p2 <- runif(1,0,1)
      
      if (p2 <= 0.25){
        winners[1] = winners[1] + 1
      }
      if (p2 > 0.25 && p2 <= 0.5){
        winners[3] = winners[3] + 1
      }
      if (p2 > 0.5 && p2 <= 0.75){
        winners[4] = winners[4] + 1
      }
      if (p2 > 0.75 && p2 <= 1){
        winners[5] = winners[5] + 1
      }
    }
    
    
    if (p1 > 0.6 && p1 <= 0.88){
      winners[3] = winners[3] + 1
      
      p2 <- runif(1,0,1)
      
      if (p2 <= 0.25){
        winners[1] = winners[1] + 1
      }
      if (p2 > 0.25 && p2 <= 0.5){
        winners[2] = winners[2] + 1
      }
      if (p2 > 0.5 && p2 <= 0.75){
        winners[4] = winners[4] + 1
      }
      if (p2 > 0.75 && p2 <= 1){
        winners[5] = winners[5] + 1
      }
    }
    
    
    if (p1 > 0.88 && p1 <= 0.98){
      winners[4] = winners[4] + 1
      
      p2 <- runif(1,0,1)
      
      if (p2 <= 0.25){
        winners[1] = winners[1] + 1
      }
      if (p2 > 0.25 && p2 <= 0.5){
        winners[2] = winners[2] + 1
      }
      if (p2 > 0.5 && p2 <= 0.75){
        winners[3] = winners[3] + 1
      }
      if (p2 > 0.75 && p2 <= 1){
        winners[5] = winners[5] + 1
      }
    }
    
    
    if (p1 > 0.98){
      winners[5] = winners[5] + 1
      
      p2 <- runif(1,0,1)
      
      if (p2 <= 0.25){
        winners[1] = winners[1] + 1
      }
      if (p2 > 0.25 && p2 <= 0.5){
        winners[2] = winners[2] + 1
      }
      if (p2 > 0.5 && p2 <= 0.75){
        winners[3] = winners[3] + 1
      }
      if (p2 > 0.75 && p2 <= 1){
        winners[4] = winners[4] + 1
      }
    }
}



places <- c('any',5)

for (i in 1:5){
  index <- which.max(winners)
  
  
  if (index == 1){
    name <- "Antony"
  }
  if (index == 2){
    name <- "Riz"
  }
  if (index == 3){
    name <- "Steven"
  }
  if (index == 4){
    name <- "Chadwik"
  }
  if (index == 5){
    name <- "Gary"
  }
  
  # making the vactor of the places - inserting the most likely players to win in order
  places[i] = name
  winners[index] = 0
  
}




