################################################################################
#################################### INTRO #####################################
################################################################################

# Objective: To group US counties into labor areas - called Commuting Zones (CZs) - by creating
#   clusters based on where each county's residents work.

# Background: CZs are commonly used for economic analysis. Because people change
#   jobs and travel different distances to work over time, CZ definitions require
#   periodic update. Different sources have made different definitions available for
#   different periods, but I have not found open source code for the clustering methodology.
#   My goal is to create open source code to create CZ definitions using the most recent
#   clustering packages.

# Links:
#   Background on CZs: https://www.aeaweb.org/conference/2017/preliminary/paper/thT52i7D
#   Cornell paper for Formulas: https://digitalcommons.ilr.cornell.edu/cgi/viewcontent.cgi?article=1044&context=ldi
#   USDA CZ definitions: https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/

################################################################################
################################## LIBRARIES ###################################
################################################################################

library(dplyr)
library(readxl)
library(tidyr)
library(cluster)

################################################################################
################################## READ DATA ###################################
################################################################################

#Load Commuting Flows data: Residence County to Workplace County Commuting Flows for the United States and Puerto Rico Sorted by Residence Geography: 5-Year ACS, 2009-2013
#Available at: https://www2.census.gov/programs-surveys/commuting/tables/time-series/commuting-flows/table1.xlsx
InputData <- read_excel("CZ/CommutingFlows.xlsx", skip = 5)

################################################################################
################################# PREPARE DATA #################################
################################################################################

# Remove NAs (these are foreign countries)
InputData <- InputData[!is.na(InputData$`County FIPS Code__1`),]

# Create State + County FIPS code
InputData$i <- paste(InputData$`State FIPS Code`, InputData$`County FIPS Code`, sep = "")
InputData$j <- paste(substr(InputData$`State FIPS Code__1`,2,3), InputData$`County FIPS Code__1`, sep = "")

# Filter for Distance analysis:
# Formula as provided by Cornell article: Dij = 1 - (Fij + Fji) / min(RLFi, RLFj)
# Distance is 1 minus the flow from i to j and from j to i, divided by the min Resident Labor Force of i or j

# Above results in negative distances when i=j and when one county has much larger population than the other
# Formula I use: Dij = 1 - (Fij + Fji) / min(TLFi, TLFj)
# Distance is 1 minus the flow from i to j and from j to i, divided by the min Total Labor Force of i or j

# Filter to i, j, Fij, Fji
Data <- InputData[,c(15,16,13)]
Data <- complete(Data, i, j) # complete missing pair values
Data[is.na(Data[,3]),3] <- 0 # replace NA with true value - 0
Data <- Data %>% left_join(Data, by=c("i"="j", "j"="i"))
colnames(Data)[3:4] <- c("Fij", "Fji")

## Calculate Total Labor Force (TLF)
# Resident Labor Force (RLF)
RLF <- Data %>% group_by(i) %>% summarise(RLF=sum(Fij))

# Non-resident Labor Force (NRLF)
NRLF <- Data %>% group_by(i) %>% summarise(NRLF=sum(Fji))

# Pull the number of people working within county to subtract from NRLF
samecounty <- subset(Data, i==j)
samecounty <- samecounty[,c(1,3)]
colnames(samecounty)[2] <- "samecounty"

# Combine for TL
TLF <- RLF %>% left_join(NRLF, by="i")
TLF <- TLF %>% left_join(samecounty, by="i")
TLF$TLF <- TLF$RLF + TLF$NRLF - TLF$samecounty

Data <- Data %>% left_join(TLF[,c(1,2,5)], by="i")
Data <- Data %>% left_join(TLF[,c(1,2,5)], by=c("j"="i"))
colnames(Data)[5:8] <- c("RLFi", "TLFi", "RLFj", "TLFj")

# Avoid double-counting workers when same county. This results in negative distances.
# For example, if 10 out of 15 workers in County X work w/in X, then Dist = 1 - (10+10)/15
Data$Fji <- ifelse(Data$i==Data$j, 0, Data$Fji)

#Calculate Distance
# Data$Dist <- 1 - ((Data$Fij + Data$Fji) / pmin(Data$RLFi, Data$RLFj))
Data$Dist <- 1 - ((Data$Fij + Data$Fji) / pmin(Data$TLFi, Data$TLFj))

# Check for Distances less than 0. Pay attention here if you change input variables above.
debug <- subset(Data, Dist < 0)
# Check whether debug issues are same-same combos or unique-unique.
debug2 <- subset(Data, Dist < 0 & i != j)


# Arrange into a matrix
Dists <- tapply(Data$Dist, list(Data$i, Data$j), sum)
isSymmetric(Dists) # Check whether the matrix is symmetric
regions <- rownames(Dists)

################################################################################
################################### CLUSTER ####################################
################################################################################

########################### Hierarchical Clustering ############################

# TS use the "average" method. So do I, while acknowledging the many other options.
fit <- hclust(as.dist(Dists), method="average")

# TS used a height of .98 that generated 741 clusters. This generates 642 at .98.
# Choosing an hclust method such as "complete" generates many more clusters.
res <- data.frame(region=regions, group=cutree(fit, h=.98))

# Can also cut by number of clusters if so desired.
# res <- data.frame(region=regions, group=cutree(fit, k=810))

# plot the result (hard to see anything because of high number of values)
plot(fit, labels=FALSE, cex.lab=0.7, cex.axis=0.7)

############################## Within Cluster SS ###############################

getWithinSS <- function(fit, k, x) {
  ss <- function(d) {
    sum(scale(d, scale = FALSE)^2)
  }

  groups <- cutree(fit, k=k)
  gdata  <- split.data.frame(x, groups)
  sum(sapply(gdata, ss))
}

plotByClust <- function(inds, ss, ...) {
  plot(inds, ss, cex=0.5, pch=19, xlab="Number of Clusters", cex.lab=0.7, cex.axis=0.7,
       mgp=c(2,0.5,0), tck=-0.01, ...
       )
}

# Sum of Squares method does not work using custom distance. Must use alternative, such as Average Silhouette.

# ss <- mapply(getWithinSS, list(fit), 1:1000, list(Dists))

# Inspect the sum of squares graph
# plotByClust(1:1000, ss, main="Sum of Squares by Number of Clusters", ylab="Sum of Squares",
            # xlim=c(0, 1000), ylim=c(0, max(ss))
            # )

############################## AVERAGE SILHOUETTE ##############################
# From Wikipedia: The silhouette value is a measure of how similar an object is to its own
# cluster (cohesion) compared to other clusters (separation). The silhouette ranges from ???1
# to +1, where a high value indicates that the object is well matched to its own cluster and
# poorly matched to neighboring clusters.

getAverageSil <- function(fit, k, dists) {
  mean(silhouette(cutree(fit, k), dists)[,3])
}

sils <- mapply(getAverageSil, list(fit), 2:3000, list(Dists))
plotByClust(2:3000, sils, main="Average Silhouette by Number of Clusters", ylab="Avg Silhouette Value")

################################ OBTAIN GROUPS #################################

# Select the number of clusters
nclust <- which.max(sils)
result <- data.frame(county=regions, group=cutree(fit, k=nclust), stringsAsFactors=FALSE)

# Add county names to result
Data2 <- InputData[,c(15, 3, 4)]
Data2 <- unique(Data2)

result <- result %>% left_join(Data2, by=c("county"="i"))
write.csv(result, "CZ/CZs.csv", row.names = F)

res <- res %>% left_join(Data2, by=c("region"="i"))
write.csv(res, "CZ/CZ_TSmethod.csv", row.names = F)