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
Data <- complete(Data, i, j) # NOTE: complete missing pair values
Data[is.na(Data[,3]),3] <- 0 # NOTE: replace NA with true value - 0
Data <- Data %>% left_join(Data, by=c("i"="j", "j"="i"))
colnames(Data)[3:4] <- c("Fij", "Fji")
RLF <- Data %>% group_by(i) %>% summarise(RLF=sum(Fij))
Data <- Data %>% left_join(RLF, by="i")
Data <- Data %>% left_join(RLF, by=c("j"="i"))
colnames(Data)[5:6] <- c("RLFi", "RLFj")
# Data$Fji <- ifelse(Data$i==Data$j, 0, Data$Fji) # avoid double-counting when same county. Issue resolved in Line 70.

#Calculate Distance
Data$Dist <- 1 - ((Data$Fij + Data$Fji) / pmin(Data$RLFi, Data$RLFj))
# 2515 combinations had distance less than 0. Most were double-counting the same county
debug <- subset(Data, Dist < 0)
# 26 combinations of two different counties had distances less than 0.
debug2 <- subset(Data, Dist < 0 & i != j)
# Minimum distance should be 0.
Data$Dist <- ifelse(Data$Dist < 0, 0, Data$Dist)


# Arrange into a matrix
Dists <- tapply(Data$Dist, list(Data$i, Data$j), sum)
isSymmetric(Dists) # NOTE: check whether the matrix is symmetric
regions <- rownames(Dists)

################################################################################
################################### CLUSTER ####################################
################################################################################

########################### Hierarchical Clustering ############################

# TS use the "average" linkage method. So do I, while noting the many other options.
fit <- hclust(as.dist(Dists), method="average")

# TS used a height of .98 that generated 741 clusters. This generates 575 at .98.
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

# Select the number of clusters (based on average silhouette (not considering first 10 points))
nclust <- which.max(sils[-c(1:10)])+10
result <- data.frame(county=regions, group=cutree(fit, k=nclust), stringsAsFactors=FALSE)

# Add county names to result
Data2 <- read_excel("CZ/CommutingFlows.xlsx", skip = 5)
Data2$fips <- paste(Data2$`State FIPS Code`, Data2$`County FIPS Code`, sep = "")
Data2 <- Data2[,c(15, 3, 4)]
Data2 <- unique(Data2)

result <- result %>% left_join(Data2, by=c("county"="fips"))
