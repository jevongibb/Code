options(scipen=999)
library(plyr)
library(tidyr)
library(dplyr)
library(caret)

#load Preferred Matrix
MultiData <- read.csv("Matrices/Multidimensional.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
EmpData <- read.csv("Matrices/AvgDist_Emp_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
EstData <- read.csv("Matrices/AvgDist_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#Reshape MultiData
MultiData$Industry <- rownames(MultiData)
MultiData <- gather(MultiData, Neighbour, dist, -Industry)
MultiData <- MultiData[MultiData$Industry != MultiData$Neighbour, ]

#Reshape EmpData
EmpData$Industry <- rownames(EmpData)
EmpData <- gather(EmpData, Neighbour, dist, -Industry)
EmpData <- EmpData[EmpData$Industry != EmpData$Neighbour, ]

#Reshape EstData
EstData$Industry <- rownames(EstData)
EstData <- gather(EstData, Neighbour, dist, -Industry)
EstData <- EstData[EstData$Industry != EstData$Neighbour, ]

#cutpoints using quantile (decision boundary)
cuts <- c(.45)
NewEmp <- EmpData
NewEmp$dist <- ifelse(NewEmp$dist>(as.numeric(quantile(NewEmp$dist, cuts))),NA,NewEmp$dist)

#Reverse distance such that bigger is closer (Switch to weight)
NewEmp$dist2 <- abs(NewEmp$dist-max(NewEmp$dist, na.rm = T))

#find total distance per node/industry
TotalDist <- NewEmp %>% group_by(Industry) %>% summarise(TotalDist=sum(dist, na.rm=T))

#newD = dist/sum
newD <- NewEmp
newD <- newD %>% left_join(TotalDist, by="Industry")
newD$newD <- newD$dist2/newD$TotalDist


#Connections = sum(dist<decision boundary)
Connections <- NewEmp %>% group_by(Industry) %>% summarise(Connections=sum(!is.na(dist)))
Connections2 <- NewEmp %>% group_by(Industry) %>% summarise(Connections=sum(dist<250, na.rm = T))

Weights <- spread(newD[,c(1,2,6)], Neighbour, newD, fill = 0)


# save KC0 and KP0 for calculations below
INS <- sweep(Weights[, 2:ncol(Weights)], 1, Connections$Connections, '*')
INS2 <- Connections[,1]
INS2$test <- rowSums(INS)












KCn_prev <- KC0$Count
KCn_minus2 <- KC0$Count
KPn_prev <- KP0$ProductCount
KPn_minus2 <- KP0$ProductCount

#run loop ## still trying to understand eigenvector alternative
count <- 0
while (count<20) { # max number of times
  count <- count+1
  
  # calculate KC(n)
  KCn <- Template
  KCn[, 2:ncol(KCn)] <- sweep(Template[, 2:ncol(Template)], 2, KPn_prev, `*`)
  KCn <- apply(KCn[, 2:ncol(KCn)], 1, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  
  # calculate KP(n)
  KPn <- Template
  KPn[, 2:ncol(KPn)] <- sweep(Template[, 2:ncol(Template)], 1, KCn_prev, `*`)
  KPn <- apply(KPn[, 2:ncol(KPn)], 2, function(x) { if (sum(x>0)==0) return(0) else return(mean(x[x>0])) })
  
#  # stop loop if delta in all KCn and KPn (for even iterations) has become less than 1
#  if (count %% 2 == 0) {
#    if (all(abs(KCn-KCn_minus2)<1) && all(abs(KPn-KPn_minus2)<1)) break 
#    KCn_minus2 <- KCn
#    KPn_minus2 <- KPn
#  }
    
  # set new values to be previous in new loop
  KCn_prev <- KCn
  KPn_prev <- KPn

}
print(paste0("Calculation was done ", count, " times"))
