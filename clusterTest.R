options(scipen=999)
library(plyr)
library(tidyr)
library(dplyr)
library(caret)

#load Preferred Matrix
MultiData <- read.csv("Matrices/Multidimensional.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
EmpData <- read.csv("Matrices/AvgDist_Emp_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
EstData <- read.csv("Matrices/AvgDist_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

test <- hclust(as.dist(MultiData), "ward.D2")

#Cluster
#Find number of clusters such that distance is minimized
#Split based on that number of clusters
#turn clusters into table of pairs
#mrge distance onto each connection


#all clusters are connected
#weight by distance

#find max,mean distance within clusters (I think I want mean)
#connect all nodes with distance < mean.cluster
#weight by distance

#make sure you don't have any big outliers that will mess up the viz

#Run for Emp and MultiData