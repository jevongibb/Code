options(scipen=999)
library(plyr)
library(tidyr)
library(dplyr)
library(caret)


#load Preferred Matrix
Data <- read.csv("Matrices/AvgDist_Emp_2015to2011.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgCor_Emp.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgCor_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Occ.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgCor_Occ.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/IO_data.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#reshape Data to long format
Data_reshaped <- Data
Data_reshaped$Industry <- rownames(Data_reshaped)
Data_reshaped <- gather(Data_reshaped, Neighbour, dist, -Industry)
Data_reshaped <- Data_reshaped[Data_reshaped$Industry != Data_reshaped$Neighbour, ]

#load Employment data
Employees <- read.csv("CZ/2015_Employees_RS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
colnames(Employees) <- c("Region", "Industry", "RS")
Employees$Industry <- as.character(Employees$Industry)


### Calculate expected RS #####################################################

optimalCut <- function(cutDist, distMat, datRS) {
  
  #filter distance matrix by cutDist (radius)
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  
  #find the industries where the minimum distance is less than the cutDist
  distMat_min_dist <- distMat %>% group_by(Industry) %>% summarise(min_dist = min(dist))
  distMat_min_dist <- distMat %>% left_join(distMat_min_dist, by = "Industry") %>% filter(dist == min_dist)
  distMat_min_dist <- distMat_min_dist[(!distMat_min_dist$Industry %in% distMat_filtered$Industry), ]
  
  #combine filtered and min_dist
  distMat_filtered <- rbind(distMat_filtered, distMat_min_dist[, c("Industry", "Neighbour", "dist")])
  
  #could not run this outside function
  eRSdat <- distMat_filtered %>% left_join(datRS, by = c("Neighbour" = "Industry"))
  
  #divide Relative Size by Distance
  eRSdat$eRSnum <- eRSdat$RS / (eRSdat$dist)
  
  #eRS is sum(RS/Dist) / sum(Dist)
  estExpectedRS <- eRSdat %>% group_by(Industry, Region) %>%
    summarise(totProx = sum(1/dist, na.rm = T),
              totRSnum = sum(eRSnum, na.rm = T)) %>%
    ungroup() %>% mutate(eRS = totRSnum / totProx)

  #create a dataframe for predictions
  RegionIndExpRS <- estExpectedRS[, c("Industry", "Region", "eRS")]

  estDistance <- estExpectedRS[, c("Industry", "Region", "eRS")] %>% left_join(datRS, by=c("Region", "Industry"))
  estDistance$RS[is.na(estDistance$RS)] <- 0
  
  #export csv of estimates for each cutDist
  write.csv(estDistance, file = paste0("estRS_", round(cutDist, 2), ".csv"), row.names = F)
  
  #track predictiveness
  residuals <- estDistance$RS-estDistance$eRS
  rmse <- sqrt(sum(residuals^2) / nrow(estDistance))
  #rmse2 <- caret::RMSE(estDistance$eRS, estDistance$RS)
  r_squared <- 1 - sum(residuals^2) / sum((estDistance$eRS - mean(estDistance$eRS))^2)
  #r_2 <- caret::R2(estDistance$eRs, estDistance$RS)
  mean_error <- sum(abs(residuals)) / nrow(estDistance)
  return(data.frame(cutDist = cutDist, rmse = rmse, r_squared = r_squared, mean_error = mean_error))
  
}

#cutpoints using Euclidean Distance (lower is closer)
cutpoints <- as.numeric(quantile(as.matrix(Data), c(.01, .03, .05, .07, .1, .15, .2, .25, .3, .4, .5)))

#cutpoints using Correlation (higher is closer)
#cutpoints <- as.numeric(quantile(Data, c(.99, .97, .95, .93, .9, .85, .8, .75, .7, .6, .5)))


# Run KNN
plyr::adply(cutpoints, 1, optimalCut, distMat = Data_reshaped, datRS = Employees)
