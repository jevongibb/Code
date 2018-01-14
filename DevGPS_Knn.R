options(scipen=999)
library(plyr)
library(dplyr)
library(tidyr)


#load Preferred Matrix
Data <- read.csv("Matrices/AvgDist_Emp.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgCor_Emp.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgCor_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Occ.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgCor_Occ.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/IO_data.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#load Employment data
Employees <- read.csv("CZ/2010_Employees_RS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
colnames(Employees) <- c("Region", "Industry", "RS")
Employees$Industry <- as.character(Employees$Industry)

#Pull required data for functions below
allIndustry <- rownames(Data)
Data <- as.matrix(Data)


### Calculate expected RS #####################################################

topKindustry <- function(industryID, distMat, datRS, radius){
  #remove the current industry from distance matrix (it will always be its own closest neighbor)
  dist <- distMat[industryID,!colnames(distMat) %in% industryID]

  #If neighbors exist within radius, use those. If not, use the closest neighbor.
  if (sum(dist<=radius) > 0) {
    distNeighbor <- dist[dist<=radius]
  } else {
    distNeighbor <- dist[dist == min(dist)]
  }
  industryK <- names(distNeighbor)
  
  topProx <- data.frame(Industry=industryK, dist=as.numeric(distNeighbor[industryK]), stringsAsFactors = F)

  #pull RS values for all neighbors within radius. Divide by distance.  
  eRSdat <- datRS[datRS$Industry %in% industryK,]
  eRSdat <- eRSdat %>% left_join(topProx, by=c("Industry"="Industry"))
  eRSdat$eRSnum <- eRSdat$RS/eRSdat$dist

  #calculate Expected RS by dividing totals from above by total distance.  
  estExpectedRS <- plyr::ddply(.data = eRSdat, .variables = "Region", summarize,
                               totProx = sum(1/dist, na.rm = T),
                               totRSnum = sum(eRSnum, na.rm = T))
  estExpectedRS$eRS <- estExpectedRS$totRSnum / estExpectedRS$totProx
  RegionIndExpRS <- data.frame(Industry=industryID, estExpectedRS[c("Region","eRS")], stringsAsFactors = F)
  rm(estExpectedRS, eRSdat, topProx, industryK)
  
  return(RegionIndExpRS)
}


optimalRadius <- function(industryList, radius, distMat, datRS){
  estDistance <- plyr::adply(industryList, 1, topKindustry, radius = radius, distMat = distMat, datRS= datRS)
  estDistance <- estDistance %>% left_join(datRS, by=c("Region", "Industry") )
  estDistance$RS[is.na(estDistance$RS)]<-0
  estDistance$X1<-NULL
  write.csv(estDistance, file = paste("estRS_",paste(round(radius, 2)), paste(".csv"),sep = ""),row.names = F)
  
  rmse <- sqrt(1/nrow(estDistance)*t(estDistance$RS - estDistance$eRS) %*% (estDistance$RS - estDistance$eRS))
  return(rmse)
}

#radiusTest using Euclidean Distance (lower is closer)
radiusTest <- as.numeric(quantile(Data, c(.05, .1, .2, .3, .4)))

#radiusTest using Correlation (higher is closer)
#radiusTest <- as.numeric(quantile(Data, c(.99, .97, .95, .93, .9, .85, .8, .75, .7, .6, .5)))


# Run KNN
plyr::aaply(radiusTest, 1, optimalRadius, industryList=allIndustry, distMat=Data, datRS=Employees)
