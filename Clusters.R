library(dplyr)
library(cluster)

Distance <- read.csv("Matrices/AvgDist_Emp_2015to2011.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

# K-Means Cluster Analysis
fit <- pam(Distance)
Clusters <- fit$clustering

write.csv(Clusters, "clusters.csv")