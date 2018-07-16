options(scipen=999)
library(tidyr)
library(dplyr)
library(data.table)
library(igraph)
library(scales)

#load Preferred Matrix
Data <- read.csv("Matrices/Multidimensional.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#load Cluster definitions
Definitions <- read.csv("Cluster/Cluster_Definitions.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

#Filter for Traded industries
Dat <- subset(Definitions, `Traded vs Local` == "Traded")

#Remove unnecessary columns
Dat <- Dat[,c(2,6,8:11)]

#Remove NA for industry codes that do not exist in 2012
Dat <- subset(Dat, `NAICS 2012` != "#N/A")

#Remove Cluster 43 (Tobacco) and 25 (Jewelry & Silverware), b/c they have no connections
Dat <- subset(Dat, `Cluster Code` != c(43, 25))

#create data frame with connections between all industries in clusters
Clusters <- Dat[, as.data.table(t(combn(`NAICS 2012`, 2))), .(`Cluster Code`)]

#Edit column names
colnames(Clusters) <- c("Cluster", "Industry", "Neighbor")


### Edit the matrix to join distances ##########################################

#reshape Data to long format
Data_reshaped <- Data
Data_reshaped$Industry <- rownames(Data_reshaped)
Data_reshaped <- gather(Data_reshaped, Neighbor, dist, -Industry)
Data_reshaped <- Data_reshaped[Data_reshaped$Industry != Data_reshaped$Neighbor, ]

#join distances to Clusters
Connections <- Clusters %>% left_join(Data_reshaped, by=c("Industry", "Neighbor"))

#evaluates distances
summary(Connections$dist)



### Find all connections with distances less than median distance from Clusters
New <- subset(Data_reshaped, dist < median(Connections$dist))

##create MST from Data_reshaped
edges <- Data_reshaped
colnames(edges) <- c("V1", "V2", "weight")
edges$V2 <- as.character(edges$V2)
edges <- edges[edges$V1 < edges$V2, ]
#convert to graph
g <- graph_from_data_frame(edges, directed = FALSE)
#create mst (graph)
g_mst <- mst(g, algorithm = "prim")
#convert to dataframe of mst edges
edges_mst <- as.data.frame(get.edgelist(g_mst))
colnames(edges_mst) <- c("Industry", "Neighbor")
#add distance
edges_mst <- edges_mst %>% left_join(Data_reshaped, by = c("Industry", "Neighbor"))


#rbind unique values from MST, New, and Clusters
Combined <- rbind(Connections[,2:4], New, edges_mst)
Combined <- unique(Combined)


### At this point, you have all the connections. Now modify to import into Gephi.
colnames(Combined) <- c("Source", "Target", "Weight")
normal <- function(x) {(x-min(x))/(max(x)-min(x))}
Combined$Weight <- (1-normal(Combined$Weight))*100

edges <- Combined

edges$Weight <- ifelse(edges$Weight==0,1,edges$Weight) # Gephi wants min value of 1


#Pull data to add info to nodes
Info <- read.csv("Web/Master_Traded.csv", sep = ",", header = T, check.names = F)
NAICSTotal <- Info %>% group_by(naics) %>% summarise(Industry_Total=sum(`2015`))
Natl_Trend <- Info[,c(2,36)] %>% distinct(naics, .keep_all = TRUE)
wages <- read.csv("Wages/2015_wages_filtered.csv", sep = ",", header = T)
wages$naics <- as.integer(wages$naics)
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", check.names = F)

### Export to csv and json
nodes <- as.data.frame(colnames(Data))
colnames(nodes)[1] <- "Id"
nodes$Id <- as.integer(as.character(nodes$Id))
nodes <- nodes %>% left_join(NAICS[,c(1,6)], by=c("Id"="NAICS"))
nodes$group <- substr(nodes$Id,1,1)
nodes <- nodes %>% left_join(NAICSTotal, by=c("Id"="naics"))
nodes <- nodes %>% left_join(Natl_Trend, by=c("Id"="naics"))
nodes <- nodes %>% left_join(wages, by=c("Id"="naics"))

#format Natl_Trend
nodes$Natl_Trend <- percent(nodes$Natl_Trend)

#format salary
nodes$salary <- paste('$',formatC(nodes$salary, big.mark=',', format = 'f'))
nodes$salary <- substr(nodes$salary,1,(nchar(nodes$salary)-5))


#Export
write.csv(edges, "cluster_edges.csv", row.names = F)
write.csv(nodes, "cluster_nodes.csv", row.names = F)
