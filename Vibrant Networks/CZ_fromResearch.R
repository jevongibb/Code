library(dplyr)
library(readxl)
library(tidyr)
library(NbClust)
library(cluster)

################################################################################
################################## READ DATA ###################################
################################################################################

#Load Commuting Flows data: Residence County to Workplace County Commuting Flows for the United States and Puerto Rico Sorted by Residence Geography: 5-Year ACS, 2009-2013
#Available at: https://www2.census.gov/programs-surveys/commuting/tables/time-series/commuting-flows/table1.xlsx
Data <- read_excel("CZ/CommutingFlows.xlsx", skip = 5)

################################################################################
################################# PREPARE DATA #################################
################################################################################

# Remove NAs (these are foreign countries)
Data <- Data[!is.na(Data$`County FIPS Code__1`),]

# Create State + County FIPS code
Data$i <- paste(Data$`State FIPS Code`, Data$`County FIPS Code`, sep = "")
Data$j <- paste(substr(Data$`State FIPS Code__1`,2,3), Data$`County FIPS Code__1`, sep = "")

# Filter for Distance analysis:
# Dij = 1 - (Fij + Fji) / min(RLFi, RLFj)
# Distance is 1 minus the flow from i to j and from j to i, divided by the min Resident Labor Force of i or j

# Filter to i, j, Fij, RLFi, RLFj
Data <- Data[,c(15,16,13)]
Data <- Data %>% left_join(Data, by=c("i"="j", "j"="i"))
colnames(Data)[3:4] <- c("Fij", "Fji")
RLF <- Data %>% group_by(i) %>% summarise(RLF=sum(Fij))
Data <- Data %>% left_join(RLF, by="i")
Data <- Data %>% left_join(RLF, by=c("j"="i"))
colnames(Data)[5:6] <- c("RLFi", "RLFj")
Data$Fji <- ifelse(Data$i==Data$j, 0, Data$Fji)

#Calculate Distance
Data$Dist <- 1 - ((Data$Fij + Data$Fji) / pmin(Data$RLFi, Data$RLFj))

# Arrange into a matrix
Dists <- tapply(Data$Dist, list(Data$i, Data$j), sum)
Dists[is.na(Dists)] <- 1
regions <- rownames(Dists)

################################################################################
################################### CLUSTER ####################################
################################################################################

########################### Hierarchical Clustering ############################

# using "complete" method to get well-bounded clusters
fit <- hclust(Dists, method="complete")

##Error in if (is.na(n) || n > 65536L) stop("size cannot be NA nor exceed 65536") : 
##  missing value where TRUE/FALSE needed
