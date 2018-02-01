library(dplyr)
library(tidyr)
options(stringsAsFactors = F)

#load wages data
wages <- read.csv("wages_2017.csv", sep = ",", header = T)

#filter
wages <- subset(wages, own_code == 5) # Private Ownership
wages <- subset(wages, agglvl_code == 18) # 6-digit NAICS
wages <- wages[, c(3,15,38)] # NAICS, avg_salary, delta
colnames(wages) <- c("naics", "salary", "delta")

#remove rows with obvious errors (salary is never 0) affects 2 rows
wages <- subset(wages, salary>0)

rownames(wages) <- NULL

write.csv(wages, "wages_filtered.csv", row.names = F)
