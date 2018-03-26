options(scipen=999)
library(plyr)
library(tidyr)
library(dplyr)
library(caret)


#load Preferred Matrix
Data <- read.csv("Matrices/Multidimensional.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Emp_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Occ.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/IO_data.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#reshape Data to long format
Data_reshaped <- Data
Data_reshaped$Industry <- rownames(Data_reshaped)
Data_reshaped <- gather(Data_reshaped, Neighbour, dist, -Industry)
Data_reshaped <- Data_reshaped[Data_reshaped$Industry != Data_reshaped$Neighbour, ]

#The 0 distances created an error, so made them a little bigger
Data_reshaped$dist <- ifelse(Data_reshaped$dist == 0, 0.00000001, Data_reshaped$dist)


#Find average distance for each Industry
AvgDist <- Data_reshaped %>% group_by(Industry) %>% summarise(AvgDist=mean(dist))
Data_reshaped <- Data_reshaped %>% left_join(AvgDist, by=c("Neighbour"="Industry"))

D <- Data_reshaped

#Sum(Dist)
SumDist <- D %>% group_by(Industry) %>% summarise(SumDist=sum(dist))
D <- D %>% left_join(SumDist, by="Industry")

#Sum(Dist)/Dist
D$Step1 <- D$SumDist/D$dist
SumDist2 <- D %>% group_by(Industry) %>% summarise(SumDist2=sum(SumDist))
D <- D %>% left_join(SumDist2, by="Industry")

#Step1/SumDist2
D$Step2 <- D$Step1 / D$SumDist2

#Step2 * AvgDist
D$Step3 <- D$Step2 * D$AvgDist

#Calc
Calc <- D %>% group_by(Industry) %>% summarise(R1=sum(Step3))

#Add labels
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
NAICS <- NAICS[,c(1,6)]
NAICS$NAICS <- as.character(NAICS$NAICS)
Calc <- Calc %>% left_join(NAICS, by=c("Industry"="NAICS"))

AvgDist <- AvgDist %>% left_join(NAICS, by=c("Industry"="NAICS"))
