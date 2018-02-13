library(dplyr)
library(reshape2)
library(igraph)
library(rgexf)
options(stringsAsFactors = FALSE)

### Load Combo Matrix
Matrix <- read.csv("Matrices/NewCombo.csv", header = T, sep = ",", check.names = F, row.names = 1)
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", check.names = F)

### Create graph
#create dataframe of edges
edges <- Matrix
edges$NAICS <- colnames(edges)
edges <- melt(edges)
colnames(edges) <- c("V1", "V2", "weight")
edges$V2 <- as.character(edges$V2)
edges <- edges[edges$V1 < edges$V2, ]
#convert to graph
g <- graph_from_data_frame(edges, directed = FALSE)


### Create a minimum spanning tree for graph
#create mst (graph)
g_mst <- mst(g, algorithm = "prim")
#convert to dataframe of mst edges
edges_mst <- as.data.frame(get.edgelist(g_mst))


### Find radius such that average number of edges per node is 10
#we just calculate how many edges we should have (= number of nodes * 10), and select edges with lowest weight
n_nodes <- ncol(Matrix)
new_edges <- edges %>% arrange(weight) %>% top_n((n_nodes*20)-675, -weight)

### Add edges within radius above (cannot rbind edges_mst and new_edges, b/c mst does not have weight)
#mark which edges in old graph and in new graph are in mst
edges_mst$in_mst <- TRUE
edges <- edges %>% left_join(edges_mst, by = c("V1", "V2"))
new_edges <- new_edges %>% left_join(edges_mst, by = c("V1", "V2"))

#add mst edges to new graph (all that it did not contain earlier)
new_edges <- rbind(new_edges[is.na(new_edges$in_mst), ], edges[!is.na(edges$in_mst), ])
new_edges$in_mst <- NULL

### Gephi requires positive numbers for weight. Normalize to create positive numbers.
normal <- function(x) {(x-min(x))/(max(x)-min(x))}
new_edges$weight <- (1-normal(new_edges$weight))*100

edges <- new_edges
colnames(edges) <- c("Source", "Target", "Weight")
edges$Weight <- ifelse(edges$Weight==0,1,edges$Weight) # Gephi wants min value of 1


#Pull data to add info to nodes
Info <- read.csv("Web/Master_Traded.csv", sep = ",", header = T, check.names = F)
NAICSTotal <- Info %>% group_by(naics) %>% summarise(Industry_Total=sum(`2015`))
Natl_Trend <- Info[,c(2,36)] %>% distinct(naics, .keep_all = TRUE)
wages <- read.csv("wages_filtered.csv", sep = ",", header = T)
wages$naics <- as.integer(wages$naics)

### Export to csv and json
nodes <- as.data.frame(colnames(Matrix))
colnames(nodes)[1] <- "Id"
nodes$Id <- as.integer(nodes$Id)
nodes <- nodes %>% left_join(NAICS[,c(1,6)], by=c("Id"="NAICS"))
nodes$group <- substr(nodes$Id,1,1)
nodes <- nodes %>% left_join(NAICSTotal, by=c("Id"="naics"))
nodes <- nodes %>% left_join(Natl_Trend, by=c("Id"="naics"))
nodes <- nodes %>% left_join(wages, by=c("Id"="naics"))

#Convert Trend and Salary to Quantiles (8)
nodes$Trend_Q <- cut(nodes$Natl_Trend, quantile(nodes$Natl_Trend, probs = 0:8/8), include.lowest = T, labels = F)
nodes$Salary_Q <- cut(nodes$salary, quantile(nodes$salary, probs = 0:8/8, na.rm = T), include.lowest = T, labels = F)
nodes$Salary_Delta_Q <- cut(nodes$delta, quantile(nodes$delta, probs = 0:8/8, na.rm = T), include.lowest = T, labels = F)


write.csv(edges, "test_edges.csv", row.names = F)
write.csv(nodes, "test_nodes.csv", row.names = F)


#Add region-specific data
nodes <- read.csv("test_nodes.csv", header = T, sep = ",", check.names = F)

Austin <- read.csv("Web/Austin_Master_Traded.csv", header = T, sep = ",", check.names = F)
Tupelo <- read.csv("Web/Tupelo_Master_Traded.csv", header = T, sep = ",", check.names = F)
Detroit <- read.csv("Web/Detroit_Master_Traded.csv", header = T, sep = ",", check.names = F)

nodes <- nodes %>% left_join(Austin[,c(2,13,14)], by=c("Id"="naics"))
colnames(nodes)[c(length(nodes)-1, length(nodes))] <- c("Austin_2015", "Austin")

nodes <- nodes %>% left_join(Tupelo[,c(2,13,14)], by=c("Id"="naics"))
colnames(nodes)[c(length(nodes)-1, length(nodes))] <- c("Tupelo_2015", "Tupelo")

nodes <- nodes %>% left_join(Detroit[,c(2,13,14)], by=c("Id"="naics"))
colnames(nodes)[c(length(nodes)-1, length(nodes))] <- c("Detroit_2015", "Detroit")

#set cutoff to light up a node
cutoff <- 0.5

#change groups to adjust for RS
nodes$Austin <- ifelse(nodes$Austin>cutoff, nodes$group, 0)
nodes$Tupelo <- ifelse(nodes$Tupelo>cutoff, nodes$group, 0)
nodes$Detroit <- ifelse(nodes$Detroit>cutoff, nodes$group, 0)


nodes[,c(11:16)][is.na(nodes[,c(11:16)])] <- 0




write.csv(nodes, "test_nodes.csv", row.names = F)




#Palette
#0: #cccccc (gray)
#1: #a6761d (brown) 
#2: #242424 (black)
#3: #377eb8 (blue)
#4: #984ea3 (purple)
#5: #73c000 (green)
#6: #ff7f00 (orange)  
#7: #e31a1c (red)
#8: #e6ab02 (gold)