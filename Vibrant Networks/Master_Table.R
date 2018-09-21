library(dplyr)
options(scipen=999)

#Load Historical Employees by CZ or County
#Employees <- read.csv("CZ/Hist_Employees_Q.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
Employees <- read.csv("County/Imputed_Concord.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

#Reorganize to start with 2005 and finish with 2015
Employees <- Employees[,c(1,2,13,12,11,10,9,8,7,6,5,4,3)]
Trends <- Employees

#Calculate 2015 RS
EmpCZTotal <- Employees %>% group_by(CZ) %>% summarise(CZ_Total=sum(`2015`))
EmpNAICSTotal <- Employees %>% group_by(naics) %>% summarise(Industry_Total=sum(`2015`))
EmpTotal <- sum(EmpNAICSTotal$Industry_Total)

Employees <- Employees %>% left_join(EmpCZTotal, by="CZ")
Employees <- Employees %>% left_join(EmpNAICSTotal, by="naics")

Employees$RS_2015 <- (Employees$`2015` / Employees$CZ_Total) / (Employees$Industry_Total / EmpTotal)
Employees$CZ_Total <- Employees$Industry_Total <- NULL

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
Natl_Trends <- Trends[,c(2:13)]
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
Master <- cbind(Employees, Master[,c(14:35)])

#Add NAICS description
NAICS <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
NAICS <- NAICS[,c(1,6)]
Master <- Master %>% left_join(NAICS, by=c("naics"="NAICS"))

### Filter by Traded and Local ################################################

#Load Traded industries
Traded <- read.csv("NAICS/traded.csv", header = T, sep = ",", stringsAsFactors = F)
Traded <- Traded[,c(1,7)]

#Filter Traded industries
Master <- Master %>% left_join(Traded, by=c("naics"="NAICS"))
Master_Traded <- subset(Master, Traded_Local == "Traded")
Master_Traded$Traded_Local <- NULL

Master_Local <-  subset(Master, Traded_Local == "Local")
Master_Local$Traded_Local <- NULL


### Export ####################################################################
write.csv(Master_Traded, "Web/Master_Traded.csv", row.names = F)
write.csv(Master_Local, "Web/Master_Local.csv", row.names = F)
