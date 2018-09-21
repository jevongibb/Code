library(dplyr)
library(tidyr)
options(scipen=999)
options(stringsAsFactors = F)

#Load Historical Employees by CZ
Employees <- read.csv("CZ/Hist_Employees_Q.csv", header = T, sep = ",", check.names = F)

#Load Historical Employees by County
Custom_Employees <- read.csv("County/Imputed_Concord.csv", header = T, sep = ",", check.names = F)

#Load custom region definitions
Austin <- read.csv("CZ/Custom_Austin.csv", header = T, sep = ",", check.names = F)
Detroit <- read.csv("CZ/Custom_Detroit.csv", header = T, sep = ",", check.names = F)
Tupelo <- read.csv("CZ/Custom_Tupelo.csv", header = T, sep = ",", check.names = F)

#Combine regions
Custom <- rbind(Austin, Detroit, Tupelo)

#Filter employees by new regions
Custom_Employees <- Custom_Employees[,c(1,2,13,12,11,10,9,8,7,6,5,4,3)] #reorg to start w/ 2005 and finish w/ 2015
Custom_Employees <- subset(Custom_Employees, County %in% Custom$County)
Custom_Employees <- Custom_Employees %>% left_join(Custom[,1:2], by = "County")
Custom_Employees <- Custom_Employees[,c(14,2:13)]
Custom_Employees <- Custom_Employees %>% group_by(Region, naics) %>% summarise_all((funs(sum(., na.rm = TRUE))))

#Reorganize to start with 2005 and finish with 2015
Employees <- Employees[,c(1,2,13,12,11,10,9,8,7,6,5,4,3)]
Trends <- Custom_Employees

#Combine Custom regions and all CZs in order to generate accurate Relative Size
REmp <- Employees
colnames(REmp)[1] <- "Region"
REmp$Region <- as.character(REmp$Region)
Combo_Employees <- bind_rows(Custom_Employees, REmp)

#Calculate 2015 RS
#EmpCZTotal <- Employees %>% group_by(CZ) %>% summarise(CZ_Total=sum(`2015`))
EmpCustomTotal <- Combo_Employees %>% group_by(Region) %>% summarise(Region_Total=sum(`2015`))
EmpNAICSTotal <- Combo_Employees %>% group_by(naics) %>% summarise(Industry_Total=sum(`2015`))
EmpTotal <- sum(EmpNAICSTotal$Industry_Total)

Combo_Employees <- Combo_Employees %>% left_join(EmpCustomTotal, by="Region")
Combo_Employees$Pct_Total <- Combo_Employees$`2015`/Combo_Employees$Region_Total
Combo_Employees <- Combo_Employees %>% left_join(EmpNAICSTotal, by="naics")
Combo_Employees$RS_2015 <- (Combo_Employees$`2015` / Combo_Employees$Region_Total) / (Combo_Employees$Industry_Total / EmpTotal)
Combo_Employees$Region_Total <- Combo_Employees$Industry_Total <- NULL

#Revert Combo to Custom, b/c done with RS
Combo_Employees <- subset(Combo_Employees, Region %in% Custom_Employees$Region)
Custom_Employees <- Combo_Employees

### Local Data ###############################################################

#Create Historical Deltas
Trends$L_T_2006 <- ifelse(Trends$`2005`>0, (Trends$`2006`/Trends$`2005`)-1, NA)
Trends$L_T_2007 <- ifelse(Trends$`2006`>0, (Trends$`2007`/Trends$`2006`)-1, NA)
Trends$L_T_2008 <- ifelse(Trends$`2007`>0, (Trends$`2008`/Trends$`2007`)-1, NA)
Trends$L_T_2009 <- ifelse(Trends$`2008`>0, (Trends$`2009`/Trends$`2008`)-1, NA)
Trends$L_T_2010 <- ifelse(Trends$`2009`>0, (Trends$`2010`/Trends$`2009`)-1, NA)
Trends$L_T_2011 <- ifelse(Trends$`2010`>0, (Trends$`2011`/Trends$`2010`)-1, NA)
Trends$L_T_2012 <- ifelse(Trends$`2011`>0, (Trends$`2012`/Trends$`2011`)-1, NA)
Trends$L_T_2013 <- ifelse(Trends$`2012`>0, (Trends$`2013`/Trends$`2012`)-1, NA)
Trends$L_T_2014 <- ifelse(Trends$`2013`>0, (Trends$`2014`/Trends$`2013`)-1, NA)
Trends$L_T_2015 <- ifelse(Trends$`2014`>0, (Trends$`2015`/Trends$`2014`)-1, NA)

#Create Local Trend (Linear Estimate)
data <- as.matrix(t(Trends[, 14:23]))
data[is.na(data)] <- 0
design.mat <- cbind(1, 1:nrow(data))

reg <- lm.fit(design.mat, data)$coefficients
Trends$Local_Trend <- reg[2,]

### Nat'l Data ###############################################################

#Create Nat'l Delta
Natl_Trends <- Employees[,c(2:13)]
Natl_Trends <- Natl_Trends %>% group_by(naics) %>% summarise_all(funs(sum(., na.rm = TRUE)))

Natl_Trends$N_T_2006 <- ifelse(Natl_Trends$`2005`>0, (Natl_Trends$`2006`/Natl_Trends$`2005`)-1, NA)
Natl_Trends$N_T_2007 <- ifelse(Natl_Trends$`2006`>0, (Natl_Trends$`2007`/Natl_Trends$`2006`)-1, NA)
Natl_Trends$N_T_2008 <- ifelse(Natl_Trends$`2007`>0, (Natl_Trends$`2008`/Natl_Trends$`2007`)-1, NA)
Natl_Trends$N_T_2009 <- ifelse(Natl_Trends$`2008`>0, (Natl_Trends$`2009`/Natl_Trends$`2008`)-1, NA)
Natl_Trends$N_T_2010 <- ifelse(Natl_Trends$`2009`>0, (Natl_Trends$`2010`/Natl_Trends$`2009`)-1, NA)
Natl_Trends$N_T_2011 <- ifelse(Natl_Trends$`2010`>0, (Natl_Trends$`2011`/Natl_Trends$`2010`)-1, NA)
Natl_Trends$N_T_2012 <- ifelse(Natl_Trends$`2011`>0, (Natl_Trends$`2012`/Natl_Trends$`2011`)-1, NA)
Natl_Trends$N_T_2013 <- ifelse(Natl_Trends$`2012`>0, (Natl_Trends$`2013`/Natl_Trends$`2012`)-1, NA)
Natl_Trends$N_T_2014 <- ifelse(Natl_Trends$`2013`>0, (Natl_Trends$`2014`/Natl_Trends$`2013`)-1, NA)
Natl_Trends$N_T_2015 <- ifelse(Natl_Trends$`2014`>0, (Natl_Trends$`2015`/Natl_Trends$`2014`)-1, NA)

#Create Nat'l Trend (Linear Estimate)
data <- as.matrix(t(Natl_Trends[, 13:22]))
data[is.na(data)] <- 0
design.mat <- cbind(1, 1:nrow(data))

reg <- lm.fit(design.mat, data)$coefficients
Natl_Trends$Natl_Trend <- reg[2,]
Natl_Trends <- Natl_Trends[,-c(2:12)]




#Join Local and Nat'l
Master <- Trends %>% left_join(Natl_Trends, by="naics")
Custom_Master <- Custom_Employees %>% left_join(Master[,c(1,2,14:35)], by=c("Region", "naics"))

#Insert a column for 5-yr Projected # of Employees (makes the visual work)
Custom_Master$`2020` <- Custom_Master$`2015` * ((1+Custom_Master$Local_Trend)^5)
Custom_Master$`2020` <- ifelse(Custom_Master$`2020`>0, round(Custom_Master$`2020`, 0), 0)
Custom_Master <- Custom_Master[,c(1:13,38,14:37)]

#Add NAICS description
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
NAICS <- NAICS[,c(1,6)]
Custom_Master <- Custom_Master %>% left_join(NAICS, by=c("naics"="NAICS"))

#Add group for color legend
Custom_Master$Group <- substr(Custom_Master$naics,1,1)

### Filter by Traded and Local ################################################

#Load Traded industries
Traded <- read.csv("NAICS/traded.csv", header = T, sep = ",", stringsAsFactors = F)
Traded <- Traded[,c(1,7)]

#Filter Traded industries
Custom_Master <- Custom_Master %>% left_join(Traded, by=c("naics"="NAICS"))
Custom_Master_Traded <- subset(Custom_Master, Traded_Local == "Traded")
Custom_Master_Traded$Traded_Local <- NULL

Custom_Master_Local <-  subset(Custom_Master, Traded_Local == "Local")
Custom_Master_Local$Traded_Local <- NULL

### Split by Region and Export ################################################

Master_Austin_Traded <- subset(Custom_Master_Traded, Region == "Austin")
Master_Detroit_Traded <- subset(Custom_Master_Traded, Region == "Detroit")
Master_Tupelo_Traded <- subset(Custom_Master_Traded, Region == "Tupelo")

Master_Austin_Local <- subset(Custom_Master_Local, Region == "Austin")
Master_Detroit_Local <- subset(Custom_Master_Local, Region == "Detroit")
Master_Tupelo_Local <- subset(Custom_Master_Local, Region == "Tupelo")

write.csv(Master_Austin_Traded, "Web/Austin_Master_Traded.csv", row.names = F)
write.csv(Master_Austin_Local, "Web/Austin_Master_Local.csv", row.names = F)

write.csv(Master_Detroit_Traded, "Web/Detroit_Master_Traded.csv", row.names = F)
write.csv(Master_Detroit_Local, "Web/Detroit_Master_Local.csv", row.names = F)

write.csv(Master_Tupelo_Traded, "Web/Tupelo_Master_Traded.csv", row.names = F)
write.csv(Master_Tupelo_Local, "Web/Tupelo_Master_Local.csv", row.names = F)