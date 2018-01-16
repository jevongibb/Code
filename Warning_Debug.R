library(dplyr)

#Load Employees by CZ and Industry (Quantity, not RS)
Employees <- read.csv("CZ/Hist_Employees_Q.csv", sep = ",", header = T, stringsAsFactors = F, check.names = F)

#Load Employees by County and Industry (Quantity, not RS)
Emp_Cty <- read.csv("County/Imputed_Concord.csv", sep = ",", header = T, stringsAsFactors = F, check.names = F)

#Remove years before 2006 (only need 10 years of data)
Employees <- Employees[,1:12]
Emp_Cty <- Emp_Cty[,1:12]

#Add Traded Column
Traded <- read.csv("NAICS/traded.csv", header = T, sep = ",", stringsAsFactors = F)
Traded <- Traded[,c(1,7)]

#Filter Traded industries
Employees <- Employees %>% left_join(Traded, by=c("naics"="NAICS"))

#Load County information for help identifying
Counties <- read.csv("County/Counties.csv", header = T, sep = ",", stringsAsFactors = F)

#Load 2015 warnings
Warn_Fix <- read.csv("County/Warnings/2015_Warnings.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

#Merge County info onto warnings
Warn_Fix <- Warn_Fix %>% left_join(Counties, by = c("County"="Region"))

#Set counties for CZ at issue
Tup_Counties <- c(28013, 28017, 28057, 28081, 28095, 28115)

#Subset Warnings by CZ
Warn_Tup <- subset(Warn_Fix, County %in% Tup_Counties)

#Subset Employment Data by CZ
Tupelo <- subset(Employees, CZ == 166)
Tup_Cty <- subset(Emp_Cty, County %in% Tup_Counties)

#Load NAICS data
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
NAICS <- NAICS[,c(1,6)]
colnames(NAICS)[1] <- "naics"

#Merge NAICS info onto Tup_Cty
Tup_Cty <- Tup_Cty %>% left_join(NAICS, by="naics")

#problem is that Union County doesn't count for Tupelo. Consider combining with CZ 389.
# need to visualize CZs so that I can look at them