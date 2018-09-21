## Remember that you MUST ALWAYS LOAD PLYR FIRST. If you load dplyr first, this will
## generate errors, and you will make yourself crazy debugging it.

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

#load Employment data
Employees <- read.csv("CZ/2015_Employees_RS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
colnames(Employees) <- c("Region", "Industry", "RS")
Employees$Industry <- as.character(Employees$Industry)

#cutpoints using Euclidean Distance (lower is closer)
cuts <- c(.45)
cutpoints <- as.numeric(quantile(as.matrix(Data), cuts))

#cutpoints using K-neighbors
cutpoints_k <- c(2, 4, 6, 10, 16, 26, 42)


### Calculate expected RS using weighted raidus #####################################################

weightedRadius <- function(cutDist, distMat, datRS) {
  
  #filter distance matrix by cutDist (radius)
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  
  ##Must decide whether or not to include Minimums. This will dramatically increase both R-squared and RMSE. It's a tradeoff.
  #find the industries where the minimum distance is greater than the cutDist
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
  write.csv(estDistance, file = paste0("estRS_weighted_radius_", round(cutDist, 2), ".csv"), row.names = F)
  
  #track predictiveness
  rmse_and_r_2 <- postResample(estDistance$eRS, estDistance$RS)
  mean_error <- sum(abs(estDistance$RS - estDistance$eRS)) / nrow(estDistance)
  
  return(data.frame(radius = cutDist, RMSE = rmse_and_r_2["RMSE"], R2 = rmse_and_r_2["Rsquared"], Mean_Error = mean_error))
  
}


## Calculate expected RS using average of neighbors w/in radius #################

avgRadius <- function(cutDist, distMat, datRS) {
  
  #filter distance matrix by cutDist (radius)
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  
  #find the industries where the minimum distance is greater than the cutDist
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
      summarise(eRS = mean(RS))
    
  })
  
  #combining results together
  estExpectedRS <- do.call(rbind, estExpectedRS)
  
  #create a dataframe for predictions
  RegionIndExpRS <- estExpectedRS[, c("Industry", "Region", "eRS")]

  estDistance <- RegionIndExpRS %>% left_join(datRS, by=c("Region", "Industry"))
  estDistance$RS[is.na(estDistance$RS)] <- 0
  
  #export csv of estimates for each cutDist
  write.csv(estDistance, file = paste0("estRS_average_radius_", round(cutDist, 2), ".csv"), row.names = F)
  
  #track predictiveness
  rmse_and_r_2 <- postResample(estDistance$eRS, estDistance$RS)
  mean_error <- sum(abs(estDistance$RS - estDistance$eRS)) / nrow(estDistance)
  
  return(data.frame(radius = cutDist, RMSE = rmse_and_r_2["RMSE"], R2 = rmse_and_r_2["Rsquared"], Mean_Error = mean_error))
  
}


## Calculate expected RS using average of top-K neighbors #################

avgKNN <- function(k, distMat, datRS) {
  
  #filter distance matrix by top-K neighbors
  distMat_filtered <- distMat %>% group_by(Industry) %>% top_n(-k, dist)
  
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
      summarise(eRS = mean(RS))
    
  })
  
  #combining results together
  estExpectedRS <- do.call(rbind, estExpectedRS)
  
  #create a dataframe for predictions
  RegionIndExpRS <- estExpectedRS[, c("Industry", "Region", "eRS")]

  estDistance <- RegionIndExpRS %>% left_join(datRS, by=c("Region", "Industry"))
  estDistance$RS[is.na(estDistance$RS)] <- 0
  
  #export csv of estimates for each K
  write.csv(estDistance, file = paste0("estRS_k", k, ".csv"), row.names = F)
  
  #track predictiveness
  rmse_and_r_2 <- postResample(estDistance$eRS, estDistance$RS)
  mean_error <- sum(abs(estDistance$RS - estDistance$eRS)) / nrow(estDistance)
  
  return(data.frame(K = k, RMSE = rmse_and_r_2["RMSE"], R2 = rmse_and_r_2["Rsquared"], Mean_Error = mean_error))
  
}

#create empty dataframes to store results
#weightedTable <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("radius", "RMSE", "R2", "Mean_Error"))
#avgTable <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("radius", "RMSE", "R2", "Mean_Error"))
#kTable <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("K", "RMSE", "R2", "Mean_Error"))

# Run KNN
weightedTable <- plyr::adply(cutpoints, 1, weightedRadius, distMat = Data_reshaped, datRS = Employees)
weightedTable$X1 <- NULL
weightedTable$Test <- weightedTable$R2 / weightedTable$RMSE
weightedTable$Quantile <- cuts
weightedTable <- weightedTable[,c(6,1:5)]

avgTable <- plyr::adply(cutpoints, 1, avgRadius, distMat = Data_reshaped, datRS = Employees)
avgTable$X1 <- NULL
avgTable$Test <- avgTable$R2 / avgTable$RMSE
avgTable$Quantile <- cuts
avgTable <- avgTable[,c(6,1:5)]

kTable <- plyr::adply(cutpoints_k, 1, avgKNN, distMat = Data_reshaped, datRS = Employees)
kTable$X1 <- NULL
kTable$Test <- kTable$R2 / kTable$RMSE