options(scipen=999)
library(plyr)
library(tidyr)
library(dplyr)
library(caret)


#load Preferred Matrix
Data <- read.csv("Matrices/AvgDist_Emp_2015to2011.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
#Data <- read.csv("Matrices/AvgDist_Occ.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
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

#cutpoints using Euclidean Distance (lower is closer)
cutpoints <- as.numeric(quantile(as.matrix(Data), c(.01, .03, .05, .07, .1, .15, .2, .25, .3, .4, .5, .6, .7, .8, .9)))

#cutpoints using K-neighbors
cutpoints <- c(1, 2, 3, 5, 8, 13, 21, 43)


### Calculate expected RS using weighted raidus #####################################################

weightedRadius <- function(cutDist, distMat, datRS) {
  
  #filter distance matrix by cutDist (radius)
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  
  #find the industries where the minimum distance is less than the cutDist
  distMat_min_dist <- distMat %>% group_by(Industry) %>% summarise(min_dist = min(dist))
  distMat_min_dist <- distMat %>% left_join(distMat_min_dist, by = "Industry") %>% filter(dist == min_dist)
  distMat_min_dist <- distMat_min_dist[(!distMat_min_dist$Industry %in% distMat_filtered$Industry), ]
  
  #combine filtered and min_dist
  distMat_filtered <- rbind(distMat_filtered, distMat_min_dist[, c("Industry", "Neighbour", "dist")])
  
  #split data to chunks (to overcome memory allocation error)
  nsplit <- 5
  all_Industries <- unique(distMat_filtered$Industry)
  split_Industries <- split(all_Industries, cut(seq_along(all_Industries), nsplit, labels = FALSE))
  
  #running analysis by chunks
  estExpectedRS <- lapply(split_Industries, function(industries_vec) {
    
    eRSdat <- distMat_filtered %>% filter(Industry %in% industries_vec) %>% 
      left_join(datRS, by = c("Neighbour" = "Industry"))
    
    #divide Relative Size by Distance
    eRSdat$eRSnum <- eRSdat$RS / eRSdat$dist
    
    #eRS is sum(RS/Dist) / sum(Dist)
    eRSdat %>% group_by(Industry, Region) %>%
      summarise(totProx = sum(1/dist, na.rm = T),
                totRSnum = sum(eRSnum, na.rm = T)) %>%
      ungroup() %>% mutate(eRS = totRSnum / totProx)
    
  })
  
  #combining results together
  estExpectedRS <- do.call(rbind, estExpectedRS)
  
  #create a dataframe for predictions
  RegionIndExpRS <- estExpectedRS[, c("Industry", "Region", "eRS")]

  estDistance <- RegionIndExpRS %>% left_join(datRS, by=c("Region", "Industry"))
  estDistance$RS[is.na(estDistance$RS)] <- 0
  
  #export csv of estimates for each cutDist
  write.csv(estDistance, file = paste0("estRS_", round(cutDist, 2), ".csv"), row.names = F)
  
  #track predictiveness
  rmse_and_r_2 <- postResample(estDistance$eRS, estDistance$RS)
  mean_error <- sum(abs(estDistance$RS - estDistance$eRS)) / nrow(estDistance)
  
  return(data.frame(cutDist = cutDist, rmse = rmse_and_r_2["RMSE"], r_2 = rmse_and_r_2["Rsquared"], mean_error = mean_error))
  
}


## Calculate expected RS using average of neighbors w/in radius #################

avgRadius <- function(cutDist, distMat, datRS) {
  
  #filter distance matrix by cutDist (radius)
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  
  #find the industries where the minimum distance is less than the cutDist
  distMat_min_dist <- distMat %>% group_by(Industry) %>% summarise(min_dist = min(dist))
  distMat_min_dist <- distMat %>% left_join(distMat_min_dist, by = "Industry") %>% filter(dist == min_dist)
  distMat_min_dist <- distMat_min_dist[(!distMat_min_dist$Industry %in% distMat_filtered$Industry), ]
  
  #combine filtered and min_dist
  distMat_filtered <- rbind(distMat_filtered, distMat_min_dist[, c("Industry", "Neighbour", "dist")])
  
  #split data to chunks (to overcome memory allocation error)
  nsplit <- 5
  all_Industries <- unique(distMat_filtered$Industry)
  split_Industries <- split(all_Industries, cut(seq_along(all_Industries), nsplit, labels = FALSE))
  
  #running analysis by chunks
  estExpectedRS <- lapply(split_Industries, function(industries_vec) {
    
    eRSdat <- distMat_filtered %>% filter(Industry %in% industries_vec) %>% 
      left_join(datRS, by = c("Neighbour" = "Industry"))
    
    #eRS is averageRS
    eRSdat %>% group_by(Industry, Region) %>%
      summarise(totProx = sum(1/dist, na.rm = T),
                totRSnum = sum(eRSnum, na.rm = T)) %>%
      ungroup() %>% mutate(eRS = totRSnum / totProx)
    
  })
  
  #combining results together
  estExpectedRS <- do.call(rbind, estExpectedRS)
  
  #create a dataframe for predictions
  RegionIndExpRS <- estExpectedRS[, c("Industry", "Region", "eRS")]

  estDistance <- RegionIndExpRS %>% left_join(datRS, by=c("Region", "Industry"))
  estDistance$RS[is.na(estDistance$RS)] <- 0
  
  #export csv of estimates for each cutDist
  write.csv(estDistance, file = paste0("estRS_", round(cutDist, 2), ".csv"), row.names = F)
  
  #track predictiveness
  rmse_and_r_2 <- postResample(estDistance$eRS, estDistance$RS)
  mean_error <- sum(abs(estDistance$RS - estDistance$eRS)) / nrow(estDistance)
  
  return(data.frame(cutDist = cutDist, rmse = rmse_and_r_2["RMSE"], r_2 = rmse_and_r_2["Rsquared"], mean_error = mean_error))
  
}


## Calculate expected RS using average of top-K neighbors #################

avgKNN <- function(cutDist, distMat, datRS) {
  
  #filter distance matrix by cutDist (radius)
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  
  #find the industries where the minimum distance is less than the cutDist
  distMat_min_dist <- distMat %>% group_by(Industry) %>% summarise(min_dist = min(dist))
  distMat_min_dist <- distMat %>% left_join(distMat_min_dist, by = "Industry") %>% filter(dist == min_dist)
  distMat_min_dist <- distMat_min_dist[(!distMat_min_dist$Industry %in% distMat_filtered$Industry), ]
  
  #combine filtered and min_dist
  distMat_filtered <- rbind(distMat_filtered, distMat_min_dist[, c("Industry", "Neighbour", "dist")])
  
  #split data to chunks (to overcome memory allocation error)
  nsplit <- 5
  all_Industries <- unique(distMat_filtered$Industry)
  split_Industries <- split(all_Industries, cut(seq_along(all_Industries), nsplit, labels = FALSE))
  
  #running analysis by chunks
  estExpectedRS <- lapply(split_Industries, function(industries_vec) {
    
    eRSdat <- distMat_filtered %>% filter(Industry %in% industries_vec) %>% 
      left_join(datRS, by = c("Neighbour" = "Industry"))
    
    #eRS is averageRS
    eRSdat %>% group_by(Industry, Region) %>%
      summarise(totProx = sum(1/dist, na.rm = T),
                totRSnum = sum(eRSnum, na.rm = T)) %>%
      ungroup() %>% mutate(eRS = totRSnum / totProx)
    
  })
  
  #combining results together
  estExpectedRS <- do.call(rbind, estExpectedRS)
  
  #create a dataframe for predictions
  RegionIndExpRS <- estExpectedRS[, c("Industry", "Region", "eRS")]

  estDistance <- RegionIndExpRS %>% left_join(datRS, by=c("Region", "Industry"))
  estDistance$RS[is.na(estDistance$RS)] <- 0
  
  #export csv of estimates for each cutDist
  write.csv(estDistance, file = paste0("estRS_", round(cutDist, 2), ".csv"), row.names = F)
  
  #track predictiveness
  rmse_and_r_2 <- postResample(estDistance$eRS, estDistance$RS)
  mean_error <- sum(abs(estDistance$RS - estDistance$eRS)) / nrow(estDistance)
  
  return(data.frame(cutDist = cutDist, rmse = rmse_and_r_2["RMSE"], r_2 = rmse_and_r_2["Rsquared"], mean_error = mean_error))
  
}

# Run KNN
plyr::adply(cutpoints, 1, weightedRadius, distMat = Data_reshaped, datRS = Employees)
