# Objective: To group US counties into labor areas - called Commuting Zones (CZs) - by creating
#   clusters based on where each county's residents work.

# Background: CZs are commonly used for economic analysis, notably by economist David Dorn.
#   Because people change jobs and travel different distances to work over time, CZ definitions
#   require periodic update. Different sources have made different definitions available for
#   different periods, but I have not found open source code for the clustering methodology.
#   My goal is to create open source code to create CZ definitions using the most recent
#   clustering packages.

# Methodology: Prefer kmeans over hclust. Must find the optimal number k (or CZs).

# Links:
#   Background on CZs: https://www.aeaweb.org/conference/2017/preliminary/paper/thT52i7D
#   David Dorn: http://www.ddorn.net/data.htm
#   USDA CZ definitions: https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/

# Issues: (1) the large number of zeroes causes an error: kmeans does not converge.
#   There should be many zeroes for each county, because there are many more counties where residents DO NOT travel to work than where they DO travel to work.

#Load packages
library(dplyr)
library(readxl)
library(tidyr)
library(NbClust)

#Load Commuting Flows data: Residence County to Workplace County Commuting Flows for the United States and Puerto Rico Sorted by Residence Geography: 5-Year ACS, 2009-2013
#Available at: https://www2.census.gov/programs-surveys/commuting/tables/time-series/commuting-flows/table1.xlsx
Data <- read_excel("CZ/CommutingFlows.xlsx", skip = 5)

#Remove NAs (these are foreign countries)
Data <- Data[!is.na(Data$`County FIPS Code__1`),]

#Create State + County FIPS code
Data$Residence <- paste(Data$`State FIPS Code`, Data$`County FIPS Code`, sep = "")
Data$Work <- paste(substr(Data$`State FIPS Code__1`,2,3), Data$`County FIPS Code__1`, sep = "")

#Filter to Residence, Work, and Flow
Data <- Data[,c(15,16,13)]
colnames(Data)[3] <- "Flow"

#Spread table
Data <- spread(Data, Work, Flow, fill = 0)
Counties <- Data$Residence
Data2 <- Data
Data2$Residence <- NULL
Data2 <- as.matrix(Data2)

## Issues/Errors occur after this point.


#Use NbClust (range is 690 to 720 b/c previous definitions had ~710 CZs)
CommutingZones <- NbClust(Data2, distance = "euclidean", min.nc=690, max.nc=720, method = "kmeans", index = "all")

#Attempting to manually do kmeans, but still caused error.
CZ700 <- kmeans(Data2, 700, nstart = 10)