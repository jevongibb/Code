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
Data$Fji <- ifelse(Data$i==Data$j, 0, Data$Fji) # avoid double-counting when same county. Need to test whether this makes a difference.

#Calculate Distance
Data$Dist <- 1 - ((Data$Fij + Data$Fji) / pmin(Data$RLFi, Data$RLFj))

# Arrange into a matrix
Dists <- tapply(Data$Dist, list(Data$i, Data$j), sum)
isSymmetric(Dists) # NOTE: check if the matrix is symmetric
regions <- rownames(Dists)

################################################################################
################################### CLUSTER ####################################
################################################################################

########################### Hierarchical Clustering ############################

# using "complete" method to get well-bounded clusters
# NOTE: TS use the "average" linkage method, not "complete". 
fit <- hclust(as.dist(Dists), method="average")

# NOTE: TS used a height of .98 that generated 741 clusters.
# Cornell paper used .9365 to generate 810. I am not going to use height to determine clusters.
# Instead, I am comparing these results with others, and use a height to generate similar # clusters.
# After testing, .969 generate 740 clusters using Average. Using Complete, .9999999 generate 809.
res <- data.frame(region=regions, group=cutree(fit, h=.9999999))

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
  plot(inds, ss, cex=0.5, pch=19, xlab="# of clusters", cex.lab=0.7, cex.axis=0.7,
       mgp=c(2,0.5,0), tck=-0.01, ...
       )
}

# get SS for each cluster size from 1 to 1000
ss <- mapply(getWithinSS, list(fit), 1:1000, list(Dists))

# Inspect the sum of squares graph
plotByClust(1:1000, ss, main="Sum of Squares by Cluster Size", ylab="sum of squares",
            xlim=c(0, 1000), ylim=c(0, max(ss))
            )

############################## AVERAGE SILHOUETTE ##############################

getAverageSil <- function(fit, k, dists) {
  mean(silhouette(cutree(fit, k), dists)[,3])
}

sils <- mapply(getAverageSil, list(fit), 2:1000, list(Dists))
plotByClust(2:1000, sils, main="Average Silhouette by Cluster Size", ylab="mean silhouette")

################################ OBTAIN GROUPS #################################

# Select the number of clusters (based on average silhouette (not considering first 10 points))
nclust <- which.max(sils[-c(1:10)])+10
result <- data.frame(county=regions, group=cutree(fit, k=nclust))

# Add county names to result
Data2 <- read_excel("CZ/CommutingFlows.xlsx", skip = 5)
Data2$fips <- paste(Data2$`State FIPS Code`, Data2$`County FIPS Code`, sep = "")
Data2 <- Data2[,c(15, 3, 4)]
Data2 <- unique(Data2)
result$county <- as.character(result$county)
result <- result %>% left_join(Data2, by=c("county"="fips"))