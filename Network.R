library(dplyr)
library(reshape2)
library(igraph)
library(rgexf)
library(jsonlite)
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
new_edges <- edges %>% arrange(weight) %>% top_n((n_nodes*10)-675, -weight)

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






#Pull data to add info to nodes
Info <- read.csv("Web/Master_Traded.csv", sep = ",", header = T, check.names = F)
NAICSTotal <- Info %>% group_by(naics) %>% summarise(Industry_Total=sum(`2015`))
Natl_Trend <- Info[,c(2,36)] %>% distinct(naics, .keep_all = TRUE)
wages <- read.csv("wages_filtered.csv", sep = ",", header = T)
wages$naics <- as.integer(wages$naics)

### Export to csv and json
nodes <- as.data.frame(colnames(Matrix))
colnames(nodes)[1] <- "Id"
nodes$id <- as.integer(nodes$id)
nodes <- nodes %>% left_join(NAICS[,c(1,6)], by=c("Id"="NAICS"))
nodes$group <- substr(nodes$Id,1,1)
nodes <- nodes %>% left_join(NAICSTotal, by=c("Id"="naics"))
nodes <- nodes %>% left_join(Natl_Trend, by=c("Id"="naics"))
nodes <- nodes %>% left_join(wages, by=c("Id"="naics"))

edges <- new_edges
colnames(edges) <- c("Source", "Target", "Weight")

write.csv(edges, "edges.csv", row.names = F)
write.csv(nodes, "nodes.csv", row.names = F)





##### Old Code ##############################################################

nodes_json <- jsonlite::toJSON(nodes, pretty = TRUE)
write(nodes_json, file = "nodes.json")

edges_json <- jsonlite::toJSON(edges, pretty = TRUE)
write(edges_json, file = "edges.json")




### Export to Gephi
#convert to graph
new_g <- graph_from_data_frame(new_edges, directed = TRUE)
#add NAICS label to nodes
V(new_g)$Industry <- NAICS$Label[match(as.numeric(V(new_g)$name), NAICS$NAICS)]
V(new_g)$Industry <- iconv(V(new_g)$Industry, "latin1", "UTF-8") #recoding to UTF-8 format (cause Gephi demands it)

#convert to Gephi format
new_g_gephi <- igraph.to.gexf(new_g)
#export
print(new_g_gephi, "new_graph.gexf", replace = TRUE)
