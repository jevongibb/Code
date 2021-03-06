library(jsonlite)
library(dplyr)
library(tidyr)

setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

#load data
data <- jsonlite::fromJSON("C:/Users/Jevon/Desktop/Vibrant/national.json")
node_data <- read.csv("nodes.csv", sep = ",", header = T, stringsAsFactors = F, check.names = F)
Local <- read.csv("Web/LafayetteCounty_Master_Traded.csv", header = T, sep = ",", check.names = F)

#the JSON has two tables: Nodes and Edges

#pull the Nodes table
nodes <- data[[1]]

#Nodes has an attributes table nested within each node

#pull the Attributes table
data_attr <- nodes$attributes


### This part shows what I want to do #################################################
# I want to change the attributes to the following columns:
# NAICS, Employees, Relative Size, Average Wage, Local Trend, National Trend

#So, rebuild based on id, which is the row number for each industry by NAICS order
newnodes <- nodes[,1:4]
newnodes$id <- as.integer(newnodes$id)
newnodes <- newnodes[order(newnodes$id),]
rownames(newnodes) <- NULL

#add the NAICS column from node_data
newnodes$NAICS <- node_data$NAICS

#add Local data (Employees, Relative Size, Local Trend, National Trend)
newnodes <- newnodes %>% left_join(Local[,c(2,17,20,31,42)], by=c("NAICS"="naics"))
newnodes[is.na(newnodes)] <- 0
colnames(newnodes)[c((length(newnodes)-3):(length(newnodes)))] <- c("Employees", "Relative Size", "Local Trend", "National Trend")

#add Wage
wages <- read.csv("Wages/2015_wages_filtered.csv", sep = ",", header = T)
wages$naics <- as.integer(wages$naics)
newnodes <- newnodes %>% left_join(wages, by=c("NAICS"="naics"))

#edit formatting
newnodes$salary <- paste('$',formatC(newnodes$salary, big.mark=',', format = 'f'))
newnodes$salary <- substr(newnodes$salary,1,(nchar(newnodes$salary)-5))

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

newnodes$`Local Trend` <- percent(newnodes$`Local Trend`)
newnodes$`National Trend` <- percent(newnodes$`National Trend`)

newnodes$`Relative Size` <- round(newnodes$`Relative Size`, digits = 2)

#re-org
colnames(newnodes)[10] <- "Average Wage"
newnodes <- newnodes[,c(1:7,10,8,9)]

### Attributes are done.

### Fix some formatting issues and export as JSON ##############################
newnodes$id <- as.character(newnodes$id)

#Re-create nested tables
nodes <- nodes[order(as.integer(nodes$id)), ]
nodes$attributes <- NULL
nodes$attributes <- newnodes[, 5:10]

#color
nodes$color <- ifelse(newnodes$`Relative Size`>0.5, nodes$color, "rgb(204,204,204)")

#size
library("scales")
nodes$size <- rescale(newnodes$Employees, to=c(10,150))

data[[1]] <- nodes




data_json <- jsonlite::toJSON(data)
write(data_json, "lafayettecounty.json")
