options(scipen=999)
library(plyr)
library(tidyr)
library(dplyr)


#load Preferred Matrix
Data <- read.csv("Matrices/AvgDist_Emp.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
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
Employees <- read.csv("CZ/2010_Employees_RS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
colnames(Employees) <- c("Region", "Industry", "RS")
Employees$Industry <- as.character(Employees$Industry)


### Calculate expected RS #####################################################

optimalCut <- function(cutDist, distMat, datRS) {
  
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  distMat_min_dist <- distMat %>% group_by(Industry) %>% summarise(min_dist = min(dist))
  distMat_min_dist <- distMat %>% left_join(distMat_min_dist, by = "Industry") %>% filter(dist == min_dist)
  distMat_min_dist <- distMat_min_dist[(!distMat_min_dist$Industry %in% distMat_filtered$Industry), ]
  distMat_filtered <- rbind(distMat_filtered, distMat_min_dist[, c("Industry", "Neighbour", "dist")])
  
  eRSdat <- distMat_filtered %>% left_join(datRS, by = c("Neighbour" = "Industry"))
  eRSdat$eRSnum <- eRSdat$RS / (1 + eRSdat$dist)
  
  estExpectedRS <- eRSdat %>% group_by(Industry, Region) %>%
    summarise(totProx = sum(1/(1+dist), na.rm = T),
              totRSnum = sum(eRSnum, na.rm = T)) %>%
    ungroup() %>% mutate(eRS = totRSnum / totProx)

  RegionIndExpRS <- estExpectedRS[, c("Industry", "Region", "eRS")]

  estDistance <- estExpectedRS[, c("Industry", "Region", "eRS")] %>% left_join(datRS, by=c("Region", "Industry"))
  estDistance$RS[is.na(estDistance$RS)] <- 0
  
  write.csv(estDistance, file = paste0("estRS_", round(cutDist, 2), ".csv"), row.names = F)
  
  residuals <- estDistance$RS-estDistance$eRS
  rmse <- sqrt(sum(residuals^2) / nrow(estDistance))
  r_squared <- 1 - sum(residuals^2) / sum((estDistance$eRS - mean(estDistance$eRS))^2)
  se <- sd(residuals) / sqrt(nrow(estDistance))
  return(data.frame(cutDist = cutDist, rmse = rmse, r_squared = r_squared, se = se))
  
}

#cutpoints using Euclidean Distance (lower is closer)
cutpoints <- as.numeric(quantile(as.matrix(Data), c(.01, .03, .05, .07, .1, .15, .2, .25, .3, .4, .5)))

#cutpoints using Correlation (higher is closer)
#cutpoints <- as.numeric(quantile(Data, c(.99, .97, .95, .93, .9, .85, .8, .75, .7, .6, .5)))


# Run KNN
plyr::adply(cutpoints, 1, optimalCut, distMat = Data_reshaped, datRS = Employees)
