library(dplyr)
library(tidyr)
library(reshape2)

#load data
Employees <- read.csv("County/Imputed_Concord.csv", sep = ",", header = T, stringsAsFactors = F, check.names = F)
Establishments <- read.csv("County/Establishments_Concord.csv", sep = ",", header = T, stringsAsFactors = F, check.names = F)


### Convert Counties to CZs ####################################################
#Note: You will lose some counties in Alaska (2105  2195  2198  2230  2275)

#load CZ definition file
CZdef <- read.csv("CZ/CommutingZones.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
CZdef <- CZdef[,1:2]
colnames(CZdef)[2] <- "CZ"

#Combine Employees and CZDef, reorganize columns, then aggregate by CZ
CZ_Employees <- inner_join(Employees, CZdef, by = c("County" = "FIPS"))
CZ_Employees <- CZ_Employees[,c(length(CZ_Employees),2:(length(CZ_Employees)-1))]
CZ_Employees <- CZ_Employees %>% group_by(CZ, naics) %>% summarise_all(funs(sum(., na.rm = TRUE)))

#Same for Establishments
CZ_Establishments <- inner_join(Establishments, CZdef, by = c("County" = "FIPS"))
CZ_Establishments <- CZ_Establishments[,c(length(CZ_Establishments),2:(length(CZ_Establishments)-1))]
CZ_Establishments <- CZ_Establishments %>% group_by(CZ, naics) %>% summarise_all(funs(sum(., na.rm = TRUE)))


### Pull a single year ########################################################

#set Year: 3 is 2015, 8 is 2010, 9 is 2009...
Emp_year <- CZ_Employees[,c(1,2,3)]
colnames(Emp_year) <- c("CZ", "naics", "Quantity")
Est_year <- CZ_Establishments[,c(1,2,3)]
colnames(Est_year) <- c("CZ", "naics", "Quantity")


### Convert Quantity to Relative Size (RS) ####################################
#This scales the data, which is necessary even though the units are constant.
#Relative size adjusts quantity by region size, because some CZs are bigger than others.
#An RS of 1 means that the quantity of activity in an industry is relatively average.

#pull total number of employees by Region and Industry
EmpRegionTotal <- Emp_year %>% group_by(CZ) %>% summarise(RegionTotal=sum(Quantity))
EmpIndustryTotal <- Emp_year %>% group_by(naics) %>% summarise(IndustryTotal=sum(Quantity))

EstRegionTotal <- Est_year %>% group_by(CZ) %>% summarise(RegionTotal=sum(Quantity))
EstIndustryTotal <- Est_year %>% group_by(naics) %>% summarise(IndustryTotal=sum(Quantity))

#calculate total sum of quantity
EmpTotal <- sum(EmpIndustryTotal$IndustryTotal)
EstTotal <- sum(EstIndustryTotal$IndustryTotal) 

#join variables from above to calculate RS
Emp_year <- Emp_year %>% left_join(EmpRegionTotal, by="CZ")
Emp_year <- Emp_year %>% left_join(EmpIndustryTotal, by="naics")
Est_year <- Est_year %>% left_join(EstRegionTotal, by="CZ")
Est_year <- Est_year %>% left_join(EstIndustryTotal, by="naics")

#calculate RS
Emp_year$RS <- (Emp_year$Quantity/Emp_year$RegionTotal) / (Emp_year$IndustryTotal/EmpTotal)
Emp_year$RS[is.na(Emp_year$RS)] <- 0
Emp_year$Quantity <- Emp_year$RegionTotal <- Emp_year$IndustryTotal <- NULL

Est_year$RS <- (Est_year$Quantity/Est_year$RegionTotal) / (Est_year$IndustryTotal/EstTotal)
Est_year$RS[is.na(Est_year$RS)] <- 0
Est_year$Quantity <- Est_year$RegionTotal <- Est_year$IndustryTotal <- NULL


### Filter by Traded ##########################################################

#Load Traded industries
Traded <- read.csv("NAICS/traded.csv", header = T, sep = ",", stringsAsFactors = F)
Traded <- Traded[,c(1,7)]

#Filter Traded industries
CZ_Emp_Traded <- Emp_year %>% left_join(Traded, by=c("naics"="NAICS"))
CZ_Emp_Traded <- subset(CZ_Emp_Traded, Traded_Local == "Traded")
CZ_Emp_Traded$Traded_Local <- NULL

CZ_Est_Traded <- Est_year %>% left_join(Traded, by=c("naics"="NAICS"))
CZ_Est_Traded <- subset(CZ_Est_Traded, Traded_Local == "Traded")
CZ_Est_Traded$Traded_Local <- NULL


## Prepare a single year for Distance calculation #############################
Emp_matrix <- spread(CZ_Emp_Traded, CZ, RS, fill=0)
allIndustry <- Emp_matrix$naics
Emp_matrix$naics <- NULL
Emp_matrix <- as.matrix(Emp_matrix)

Est_matrix <- spread(CZ_Est_Traded, CZ, RS, fill=0)
Est_matrix$naics <- NULL
Est_matrix <- as.matrix(Est_matrix)


### Euclidean Distance ########################################################

Dist_Emp <- as.matrix(dist(Emp_matrix, method = "euclidean"))
rownames(Dist_Emp) <- allIndustry 
colnames(Dist_Emp) <- allIndustry

Dist_Est <- as.matrix(dist(Est_matrix, method = "euclidean"))
rownames(Dist_Est) <- allIndustry
colnames(Dist_Est) <- allIndustry


### Correlation ###############################################################

#transpose the matrix because Correlation uses relationship between columns
Emp_matrix <- t(Emp_matrix)
Cor_Emp <- as.matrix(cor(Emp_matrix, use = "pairwise.complete.obs")) #Warning message expected
Cor_Emp <- 0.5 * (1+Cor_Emp)
rownames(Cor_Emp) <- allIndustry
colnames(Cor_Emp) <- allIndustry

Est_matrix <- t(Est_matrix)
Cor_Est <- as.matrix(cor(Est_matrix, use = "pairwise.complete.obs"))
Cor_Est <- 0.5 * (1+Cor_Est)
rownames(Cor_Est) <- allIndustry
colnames(Cor_Est) <- allIndustry


### Create historical averages ################################################

##Create AvgDist dataframes for the first year/iteration, then use code below after
#AvgDist_Emp <- melt(Dist_Emp, id="CZ")
#colnames(AvgDist_Emp) <- c("Industry1","Industry2","2010")

#AvgDist_Est <- melt(Dist_Est, id="CZ")
#colnames(AvgDist_Est) <- c("Industry1","Industry2","2010")

#AvgCor_Emp <- melt(Cor_Emp, id="CZ")
#colnames(AvgCor_Emp) <- c("Industry1","Industry2","2010")

#AvgCor_Est <- melt(Cor_Est, id="CZ")
#colnames(AvgCor_Est) <- c("Industry1","Industry2","2010")


#Add columns to AvgDist and AvgCor on subsequent years/iterations
NextDist_Emp <- melt(Dist_Emp, id="CZ")
colnames(NextDist_Emp) <- c("Industry1","Industry2","2006")

NextDist_Est <- melt(Dist_Est, id="CZ")
colnames(NextDist_Est) <- c("Industry1","Industry2","2006")

AvgDist_Emp <- AvgDist_Emp %>% left_join(NextDist_Emp, by=c("Industry1", "Industry2"))
AvgDist_Est <- AvgDist_Est %>% left_join(NextDist_Est, by=c("Industry1", "Industry2"))

NextCor_Emp <- melt(Cor_Emp, id="CZ")
colnames(NextCor_Emp) <- c("Industry1","Industry2","2006")

NextCor_Est <- melt(Cor_Est, id="CZ")
colnames(NextCor_Est) <- c("Industry1","Industry2","2006")

AvgCor_Emp <- AvgCor_Emp %>% left_join(NextCor_Emp, by=c("Industry1", "Industry2"))
AvgCor_Est <- AvgCor_Est %>% left_join(NextCor_Est, by=c("Industry1", "Industry2"))






### When finished combining years #############################################
AvgDist_Emp$Avg <- rowMeans(AvgDist_Emp[,c(3:7)], na.rm = T)
AvgDist_Est$Avg <- rowMeans(AvgDist_Est[,c(3:7)], na.rm = T)
AvgCor_Emp$Avg <- rowMeans(AvgCor_Emp[,c(3:7)], na.rm = T)
AvgCor_Est$Avg <- rowMeans(AvgCor_Est[,c(3:7)], na.rm = T)

AvgDist_Emp <- AvgDist_Emp[,c(1,2,8)]
AvgDist_Est <- AvgDist_Est[,c(1,2,8)]
AvgCor_Emp <- AvgCor_Emp[,c(1,2,8)]
AvgCor_Est <- AvgCor_Est[,c(1,2,8)]

AvgDist_Emp_DF <- spread(AvgDist_Emp, Industry1, Avg, fill = 0)
AvgDist_Est_DF <- spread(AvgDist_Est, Industry1, Avg, fill = 0)
AvgCor_Emp_DF <- spread(AvgCor_Emp, Industry1, Avg, fill = 0)
AvgCor_Est_DF <- spread(AvgCor_Est, Industry1, Avg, fill = 0)

AvgDist_Emp_DF$Industry2 <- NULL
AvgDist_Est_DF$Industry2 <- NULL
AvgCor_Emp_DF$Industry2 <- NULL
AvgCor_Est_DF$Industry2 <- NULL

row.names(AvgDist_Emp_DF) <- allIndustry
row.names(AvgDist_Est_DF) <- allIndustry
row.names(AvgCor_Emp_DF) <- allIndustry
row.names(AvgCor_Est_DF) <- allIndustry


### Export ####################################################################
write.csv(AvgDist_Emp_DF, "Matrices/AvgDist_Emp.csv")
write.csv(AvgDist_Est_DF, "Matrices/AvgDist_Est.csv")
write.csv(AvgCor_Emp_DF, "Matrices/AvgCor_Emp.csv")
write.csv(AvgCor_Est_DF, "Matrices/AvgCor_Est.csv")

#Test <- read.csv("Matrices/AvgDist_Emp.csv", header = T, sep = ",", check.names = F)