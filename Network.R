library(dplyr)
library(reshape2)

#Load Combo Matrix
Matrix <- read.csv("Matrices/NewCombo.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

# Create a minimum spanning tree from combo matrix

# Find radius such that average number of edges per node is 10

### You might want to review "script.R" for this section. You made this for me last Summer.

# Add edges within radius above

# Gephi requires positive numbers for weight. How would you do this?

# Export edges file to Gephi

## Export nodes file to Gephi
# First, left join NAICS descriptions to nodes
nodes <- melt(Matrix)
nodes <- nodes[,1:2]
NAICS <- NAICS(,c(1,6))
nodes <- nodes %>% left_join(NAICS, by=c("naics"="NAICS"))

# Then export