library(dplyr)
library(tidyr)
options(stringsAsFactors = F)

#load wages data (http://www.bls.gov/cew/data/api/2016/a/area/US000.csv)
#see also (https://data.bls.gov/cew/apps/data_views/data_views.htm#tab=Tables)
wages <- read.csv("Wages/2015_wages.csv", sep = ",", header = T)

#filter
wages <- subset(wages, own_code == 5) # Private Ownership
wages <- subset(wages, agglvl_code == 18) # 6-digit NAICS
wages <- wages[, c(3,15)] # NAICS, avg_salary
colnames(wages) <- c("naics", "salary")

#remove rows with obvious errors (salary is never 0) affects 2 rows
wages <- subset(wages, salary>0)

rownames(wages) <- NULL

write.csv(wages, "Wages/2015_wages_filtered.csv", row.names = F)
