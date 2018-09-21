options(scipen=999)
library(plyr)
library(tidyr)
library(dplyr)
library(caret)




#Load the custom data set

#First, need employment data, projections, and relative size

#Load employees by Industry and CZ (for use calculating RS)
Employees <- read.csv("CZ/Hist_Employees_Q.csv", header = T, sep = ",", check.names = F)
Employees <- Employees[,c(1,2,13,12,11,10,9,8,7,6,5,4,3)]

#Load Master_Table
Master <- read.csv("Web/Master_Counties.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
Total_Data <- Master[,c(1,2,13,24,36,37)]

##Need to project to 2020

#Some Local_Trends are really big or really small. Adjust all to within the 5%-95% range.
Total_Data$New_Trend <- ifelse(Total_Data$Local_Trend > quantile(Total_Data$Local_Trend, .95), quantile(Total_Data$Local_Trend, .95), Total_Data$Local_Trend)
Total_Data$New_Trend <- ifelse(Total_Data$New_Trend < quantile(Total_Data$Local_Trend, .05), quantile(Total_Data$Local_Trend, .05), Total_Data$New_Trend)

#project
Total_Data$`2020` <- Total_Data$`2015` * ((1+Total_Data$New_Trend)^5)
Total_Data$`2020` <- ifelse(Total_Data$`2020`>0, round(Total_Data$`2020`, 0), 0)


### Create custom regions ####################
#Load custom region definitions
Custom1 <- read.csv("CZ/Custom_Orleans.csv", header = T, sep = ",", check.names = F, stringsAsFactors = F)
Custom2 <- Custom1[1,]
Custom3 <- Custom1[2,]

#Combine regions
Custom <- rbind(Custom1, Custom2, Custom3)

#Combine counties into regions
Custom_Employees <- subset(Total_Data, County %in% Custom$County)
Custom_Employees <- Custom_Employees %>% left_join(Custom[,1:2], by = "County")
Custom_Employees <- Custom_Employees[,c(9,2,3,8)]
Custom_Employees <- Custom_Employees %>% group_by(Region, naics) %>% summarise_all((funs(sum(., na.rm = TRUE))))

#Turn Total_Data into CZ_Total_Data
CZdef <- read.csv("CZ/CommutingZones.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
CZdef <- CZdef[,1:2]
colnames(CZdef)[2] <- "CZ"

#Combine Employees and CZDef, reorganize columns, then aggregate by CZ
CZ_Employees <- inner_join(Total_Data, CZdef, by = c("County" = "FIPS"))
CZ_Employees <- CZ_Employees[,c(9,2,3,8)]
CZ_Employees <- CZ_Employees %>% group_by(CZ, naics) %>% summarise_all(funs(sum(., na.rm = TRUE)))

#Combine Custom regions and all CZs in order to generate accurate Relative Size
REmp <- CZ_Employees
colnames(REmp)[1] <- "Region"
REmp$Region <- as.character(REmp$Region)
Combo_Employees <- bind_rows(Custom_Employees, REmp)

#Calculate 2020 RS
EmpCustomTotal <- Combo_Employees %>% group_by(Region) %>% summarise(Region_Total=sum(`2020`))
EmpNAICSTotal <- Combo_Employees %>% group_by(naics) %>% summarise(Industry_Total=sum(`2020`))
EmpTotal <- sum(EmpNAICSTotal$Industry_Total)

Combo_Employees <- Combo_Employees %>% left_join(EmpCustomTotal, by="Region")
Combo_Employees <- Combo_Employees %>% left_join(EmpNAICSTotal, by="naics")
Combo_Employees$EmpTotal <- EmpTotal
Combo_Employees$RS_2020 <- (Combo_Employees$`2020` / Combo_Employees$Region_Total) / (Combo_Employees$Industry_Total / EmpTotal)

#Revert Combo to Custom, b/c done with RS
Combo_Employees <- subset(Combo_Employees, Region %in% Custom_Employees$Region)
Custom_Employees <- Combo_Employees

#Get rid of Local
#Load Traded industries
Traded <- read.csv("NAICS/traded.csv", header = T, sep = ",", stringsAsFactors = F)
Traded <- Traded[,c(1,7)]

Custom_Employees <- Custom_Employees %>% left_join(Traded, by=c("naics"="NAICS"))
Custom_Employees <- subset(Custom_Employees, Traded_Local == "Traded")
Custom_Employees$Traded_Local <- NULL

#write CSV
write.csv(Custom_Employees, "CZ/CustomOrleans_forKNN.csv", row.names = F)



### Now, Custom KNN

#load Preferred Matrix
Data <- read.csv("Matrices/AvgDist_Emp_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#Decision boundary is Quantile .45
#read.csv("findOptimum.csv", header=T, sep=",", stringsAsFactors = F, check.names = F)

#reshape Data to long format
Data_reshaped <- Data
Data_reshaped$Industry <- rownames(Data_reshaped)
Data_reshaped <- gather(Data_reshaped, Neighbour, dist, -Industry)
Data_reshaped <- Data_reshaped[Data_reshaped$Industry != Data_reshaped$Neighbour, ]

#Load Custom Employment data
Employees <- Custom_Employees[,c(1,2,8)]
Employees <- subset(Employees, RS != 0)
colnames(Employees) <- c("Region", "Industry", "RS")
Employees$Industry <- as.character(Employees$Industry)

#cutpoints using Euclidean Distance (lower is closer)
cuts <- c(.45)
cutpoints <- as.numeric(quantile(as.matrix(Data), cuts))


### Calculate expected RS using weighted raidus #####################################################

weightedRadius <- function(cutDist, distMat, datRS) {
  
  #filter distance matrix by cutDist (radius)
  distMat_filtered <- distMat[distMat$dist <= cutDist, ]
  
  ##Must decide whether or not to include Minimums. This will dramatically increase both R-squared and RMSE. It's a tradeoff.
  ##If not calibrating, I choose not to include minimums. Don't try to predict the outliers.
  #find the industries where the minimum distance is greater than the cutDist
  #distMat_min_dist <- distMat %>% group_by(Industry) %>% summarise(min_dist = min(dist))
  #distMat_min_dist <- distMat %>% left_join(distMat_min_dist, by = "Industry") %>% filter(dist == min_dist)
  #distMat_min_dist <- distMat_min_dist[(!distMat_min_dist$Industry %in% distMat_filtered$Industry), ]
  
  #combine filtered and min_dist
  #distMat_filtered <- rbind(distMat_filtered, distMat_min_dist[, c("Industry", "Neighbour", "dist")])
  
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
  
  return(estDistance)
  
}

weightedTable <- plyr::adply(cutpoints, 1, weightedRadius, distMat = Data_reshaped, datRS = Employees)
weightedTable$X1 <- NULL
weightedTable <- weightedTable[,c(2,1,3,4)]
weightedTable <- weightedTable[with(weightedTable, order(Region)),]
row.names(weightedTable) <- NULL

dTable <- weightedTable
colnames(dTable)[2] <- "naics"
dTable$naics <- as.integer(dTable$naics)
#join data to dTable
Custom_Employees <- Custom_Employees %>% left_join(dTable[,1:3], by=c("Region", "naics"))

#Convert into number of employees
Custom_Employees$Prediction <- round(Custom_Employees$eRS * (Custom_Employees$Industry_Total / Custom_Employees$EmpTotal) * Custom_Employees$Region_Total, 0)

#Delta between Prediction and 2020
Custom_Employees$Delta_Net <- Custom_Employees$Prediction - Custom_Employees$`2020`

#Delta between 2020 and 2015
Custom_Employees$Delta_Linear <- Custom_Employees$`2020` - Custom_Employees$`2015`

#Add wages
wages <- read.csv("Wages/2015_wages_filtered.csv", sep = ",", header = T)
wages$naics <- as.integer(wages$naics)
Custom_Employees <- Custom_Employees %>% left_join(wages, by="naics")


### Generate Net Gains/Losses

Custom_Employees$Net_Linear <- Custom_Employees$Delta_Linear * Custom_Employees$salary
Custom_Employees$Net_Network <- Custom_Employees$Delta_Net * Custom_Employees$salary

# Add labels for ease of analysis
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
NAICS <- NAICS[,c(1,6)]
Custom_Employees <- Custom_Employees %>% left_join(NAICS, by=c("naics"="NAICS"))



### Convert into SWOT ###########################################
SWOT <- Custom_Employees


# Oxford
Oxford_SWOT_strengths <- subset(SWOT, Region == "Oxford")
Oxford_SWOT_strengths <- Oxford_SWOT_strengths[,c(16, 12, 13, 14)]
colnames(Oxford_SWOT_strengths) <- c("Description", "People(+)", "Avg Wage", "Net Gain")
Oxford_SWOT_strengths <- Oxford_SWOT_strengths[with(Oxford_SWOT_strengths, order(-`Net Gain`)),]
Oxford_SWOT_strengths <- Oxford_SWOT_strengths[1:10,]
#formatting
Oxford_SWOT_strengths$`Avg Wage` <- paste('$',formatC(Oxford_SWOT_strengths$`Avg Wage`, big.mark=',', format = 'f'))
Oxford_SWOT_strengths$`Avg Wage` <- substr(Oxford_SWOT_strengths$`Avg Wage`,1,(nchar(Oxford_SWOT_strengths$`Avg Wage`)-5))
Oxford_SWOT_strengths$`People(+)` <- prettyNum(Oxford_SWOT_strengths$`People(+)`, big.mark = ",")
Oxford_SWOT_strengths$`Net Gain` <- paste('$',formatC(Oxford_SWOT_strengths$`Net Gain`, big.mark=',', format = 'f'))
Oxford_SWOT_strengths$`Net Gain` <- substr(Oxford_SWOT_strengths$`Net Gain`,1,(nchar(Oxford_SWOT_strengths$`Net Gain`)-5))


Oxford_SWOT_weaknesses <- subset(SWOT, Region == "Oxford")
Oxford_SWOT_weaknesses <- Oxford_SWOT_weaknesses[,c(16, 12, 13, 14)]
colnames(Oxford_SWOT_weaknesses) <- c("Description", "People(-)", "Avg Wage", "Net Loss")
Oxford_SWOT_weaknesses <- Oxford_SWOT_weaknesses[with(Oxford_SWOT_weaknesses, order(`Net Loss`)),]
Oxford_SWOT_weaknesses <- Oxford_SWOT_weaknesses[1:10,]
#formatting
Oxford_SWOT_weaknesses$`Avg Wage` <- paste('$',formatC(Oxford_SWOT_weaknesses$`Avg Wage`, big.mark=',', format = 'f'))
Oxford_SWOT_weaknesses$`Avg Wage` <- substr(Oxford_SWOT_weaknesses$`Avg Wage`,1,(nchar(Oxford_SWOT_weaknesses$`Avg Wage`)-5))
Oxford_SWOT_weaknesses$`People(-)` <- prettyNum(Oxford_SWOT_weaknesses$`People(-)`, big.mark = ",")
Oxford_SWOT_weaknesses$`Net Loss` <- paste('$',formatC(Oxford_SWOT_weaknesses$`Net Loss`, big.mark=',', format = 'f'))
Oxford_SWOT_weaknesses$`Net Loss` <- substr(Oxford_SWOT_weaknesses$`Net Loss`,1,(nchar(Oxford_SWOT_weaknesses$`Net Loss`)-5))


Oxford_SWOT_opportunities <- subset(SWOT, Region == "Oxford")
Oxford_SWOT_opportunities <- Oxford_SWOT_opportunities[,c(16, 11, 13, 15)]
colnames(Oxford_SWOT_opportunities) <- c("Description", "People(+)", "Avg Wage", "Net Gain")
Oxford_SWOT_opportunities <- Oxford_SWOT_opportunities[with(Oxford_SWOT_opportunities, order(-`Net Gain`)),]
Oxford_SWOT_opportunities <- Oxford_SWOT_opportunities[1:10,]
#formatting
Oxford_SWOT_opportunities$`Avg Wage` <- paste('$',formatC(Oxford_SWOT_opportunities$`Avg Wage`, big.mark=',', format = 'f'))
Oxford_SWOT_opportunities$`Avg Wage` <- substr(Oxford_SWOT_opportunities$`Avg Wage`,1,(nchar(Oxford_SWOT_opportunities$`Avg Wage`)-5))
Oxford_SWOT_opportunities$`People(+)` <- prettyNum(Oxford_SWOT_opportunities$`People(+)`, big.mark = ",")
Oxford_SWOT_opportunities$`Net Gain` <- paste('$',formatC(Oxford_SWOT_opportunities$`Net Gain`, big.mark=',', format = 'f'))
Oxford_SWOT_opportunities$`Net Gain` <- substr(Oxford_SWOT_opportunities$`Net Gain`,1,(nchar(Oxford_SWOT_opportunities$`Net Gain`)-5))

Oxford_SWOT_threats <- subset(SWOT, Region == "Oxford")
Oxford_SWOT_threats <- Oxford_SWOT_threats[,c(16, 11, 13, 15)]
colnames(Oxford_SWOT_threats) <- c("Description", "People(-)", "Avg Wage", "Net Loss")
Oxford_SWOT_threats <- Oxford_SWOT_threats[with(Oxford_SWOT_threats, order(`Net Loss`)),]
Oxford_SWOT_threats <- Oxford_SWOT_threats[1:10,]
#formatting
Oxford_SWOT_threats$`Avg Wage` <- paste('$',formatC(Oxford_SWOT_threats$`Avg Wage`, big.mark=',', format = 'f'))
Oxford_SWOT_threats$`Avg Wage` <- substr(Oxford_SWOT_threats$`Avg Wage`,1,(nchar(Oxford_SWOT_threats$`Avg Wage`)-5))
Oxford_SWOT_threats$`People(-)` <- prettyNum(Oxford_SWOT_threats$`People(-)`, big.mark = ",")
Oxford_SWOT_threats$`Net Loss` <- paste('$',formatC(Oxford_SWOT_threats$`Net Loss`, big.mark=',', format = 'f'))
Oxford_SWOT_threats$`Net Loss` <- substr(Oxford_SWOT_threats$`Net Loss`,1,(nchar(Oxford_SWOT_threats$`Net Loss`)-5))


write.csv(Oxford_SWOT_strengths, "Web/Oxford_SWOT_strengths.csv", row.names = F)
write.csv(Oxford_SWOT_weaknesses, "Web/Oxford_SWOT_weaknesses.csv", row.names = F)
write.csv(Oxford_SWOT_opportunities, "Web/Oxford_SWOT_opportunities.csv", row.names = F)
write.csv(Oxford_SWOT_threats, "Web/Oxford_SWOT_threats.csv", row.names = F)