################################################################################
#################################### INTRO #####################################
################################################################################

# Objective: To group US counties into labor areas - called Commuting Zones (CZs) - by creating
#   clusters based on where each county's residents work.

# Background: CZs are commonly used for economic analysis, notably by economist David Dorn.
#   Because people change jobs and travel different distances to work over time, CZ definitions
#   require periodic update. Different sources have made different definitions available for
#   different periods, but I have not found open source code for the clustering methodology.
#   My goal is to create open source code to create CZ definitions using the most recent
#   clustering packages.

# Links:
#   Background on CZs: https://www.aeaweb.org/conference/2017/preliminary/paper/thT52i7D
#   David Dorn: http://www.ddorn.net/data.htm
#   USDA CZ definitions: https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/

################################################################################
################################## LIBRARIES ###################################
################################################################################

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
Data$Residence <- paste(Data$`State FIPS Code`, Data$`County FIPS Code`, sep = "")
Data$Work <- paste(substr(Data$`State FIPS Code__1`,2,3), Data$`County FIPS Code__1`, sep = "")

# Filter to Residence, Work, and Flow
Data <- Data[,c(15,16,13)]
colnames(Data)[3] <- "Flow"

# Arrange into a matrix
dat <- tapply(Data$Flow, list(Data$Residence, Data$Work), sum)
dat[is.na(dat)] <- 0
regions <- rownames(dat)

# Turn number of workers into percent of total workers
options(scipen = 999)
dat <- dat/rowSums(dat) * 100

## Move "home" to a separate column (still thinking about this)
#dat <- cbind(diag(dat), dat)
#diag(dat[,-1]) <- 0

################################################################################
################################### CLUSTER ####################################
################################################################################

########################### Hierarchical Clustering ############################

# distances between each pair of rows
dists <- dist(dat)

# using "complete" method to get well-bounded clusters
fit <- hclust(dists, method="complete")

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
ss <- mapply(getWithinSS, list(fit), 1:1000, list(dat))

# Inspect the sum of squares graph
plotByClust(1:1000, ss, main="Sum of Squares by Cluster Size", ylab="sum of squares",
            xlim=c(0, 1000), ylim=c(0, max(ss))
            )

############################## AVERAGE SILHOUETTE ##############################

getAverageSil <- function(fit, k, dists) {
  mean(silhouette(cutree(fit, k), dists)[,3])
}

sils <- mapply(getAverageSil, list(fit), 2:1000, list(dists))
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

## ISSUE: The groups make NO sense.