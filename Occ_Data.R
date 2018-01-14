library(dplyr)
library(tidyr)
library(readxl)
library(data.table)
library(reshape2)

#load data (available at https://www.bls.gov/oes/tables.htm)
occ_5d <- read_xls("OCC/2006/nat5d_M2006_dl.xls")
occ_4d <- read_xls("OCC/2006/nat4d_M2006_dl.xls")
occ_3d <- read_xls("OCC/2006/nat3d_M2006_dl.xls")

Industries <- read.csv("NAICS/NAICS.csv", sep = ",", header = T, stringsAsFactors = F, check.names = F) # From Porter

### Add Concordance if dealing with data over multiple NAICS code periods
#Concord2002 <- read.csv("NAICS/Concord_1998to2002.csv", header = T, sep = ",")
Concord2007 <- read.csv("NAICS/Concord_2002to2007.csv", header = T, sep = ",")
Concord2012 <- read.csv("NAICS/Concord_2007to2012.csv", header = T, sep = ",")

#Choose column with RELEVENT !!!!YEAR!!! NAICS values to use below (emphasis added because I forget to do this)
Industries <- as.data.frame(Industries$`NAICS 2002`)
Industries[Industries == "#N/A"] <- NA
Industries <- as.data.frame(Industries %>% drop_na())
colnames(Industries) <- "NAICS"
Industries$NAICS <- as.integer(as.character(Industries$NAICS))

#filter OCC data to relevant columns
occ_5d <- occ_5d[,c(1,3,8)]
occ_5d$PCT_TOTAL[occ_5d$PCT_TOTAL == "**"] <- 0

occ_4d <- occ_4d[,c(1,3,8)]
occ_4d$PCT_TOTAL[occ_4d$PCT_TOTAL == "**"] <- 0

occ_3d <- occ_3d[,c(1,3,8)]
occ_3d$PCT_TOTAL[occ_3d$PCT_TOTAL == "**"] <- 0


#figure out match for each industry
#6-digit
Industries$Match <- ifelse(Industries$NAICS %in% occ_5d$NAICS, Industries$NAICS, NA)

#5-digit
Industries$Match <- 
  ifelse(is.na(Industries$Match),
  ifelse(paste(substr(Industries$NAICS,1,5),0, sep = "") %in% occ_5d$NAICS, paste(substr(Industries$NAICS,1,5),0, sep = ""), NA),
  Industries$Match)

#4-digit
Industries$Match <- 
  ifelse(is.na(Industries$Match),
  ifelse(paste(substr(Industries$NAICS,1,4),"00", sep = "") %in% occ_4d$NAICS, paste(substr(Industries$NAICS,1,4),"00", sep = ""), NA),
  Industries$Match)

#3-digit
Industries$Match <- 
  ifelse(is.na(Industries$Match),
  ifelse(paste(substr(Industries$NAICS,1,3),"000", sep = "") %in% occ_3d$NAICS, paste(substr(Industries$NAICS,1,3),"000", sep = ""), NA),
  Industries$Match)

#turn occ tables wide, then merge on Match
occ_5d_wide <- spread(occ_5d, OCC_CODE, PCT_TOTAL, fill = 0)
occ_5d_wide <- occ_5d_wide[,-2]

occ_4d_wide <- spread(occ_4d, OCC_CODE, PCT_TOTAL, fill = 0)
occ_4d_wide <- occ_4d_wide[,-2]

occ_3d_wide <- spread(occ_3d, OCC_CODE, PCT_TOTAL, fill = 0)
occ_3d_wide <- occ_3d_wide[,-2]

#Industries$Match <- as.integer(Industries$Match)
Industries2 <- Industries %>% left_join(occ_5d_wide, by=c("Match"="NAICS"))
Industries2 <- subset(Industries2, Match %in% occ_5d_wide$NAICS)

Industries3 <- Industries %>% left_join(occ_4d_wide, by=c("Match"="NAICS"))
Industries3 <- subset(Industries3, Match %in% occ_4d_wide$NAICS)

Industries4 <- Industries %>% left_join(occ_3d_wide, by=c("Match"="NAICS"))
Industries4 <- subset(Industries4, Match %in% occ_3d_wide$NAICS)

Industries5 <- bind_rows(Industries2,Industries3)
Industries5 <- bind_rows(Industries5,Industries4)

#remove Match column
Industries5$Match <- NULL
Occ_Mat <- Industries5


#re-order rows and columns
Occ_Mat <- Occ_Mat[with(Occ_Mat, order(NAICS)),]

Test <- Occ_Mat[,c(2:length(Occ_Mat))]
Test <- Test[,order(colnames(Test), decreasing = F)]

Occ_Mat2 <- cbind(Occ_Mat[,1], Test)
rownames(Occ_Mat2) <- NULL
colnames(Occ_Mat2)[1] <- "NAICS"

Occ_Mat <- Occ_Mat2
Occ_Mat[,2:length(Occ_Mat)] <- sapply(Occ_Mat[2:length(Occ_Mat)], as.numeric)

##### WARNING #################################################################
# RowSums do not add up to 100. Technically, they should, b/c the data is PCT_TOTAL.
# I did not fix this, b/c child categories do not add up to parents (some data is lost or anonymized)
# and b/c I do not think this will significantly affect Correlation or Distance. In fact,
# it may make it work better, b/c we need industry-industry pairs, eliminating the OCCs in the process,
# while still benefitting from having the most information possible.

###############################################################################

Occ_Con <- Occ_Mat

#Concordance if operating in different NAICS code period.
#Concord to the latest period you will use (IE: if projecting to 2015, then 2012)
#I still need to refine this code to better deal with concordance over multiple periods.

Concord2007 <- Concord2007[,c(2,1)]
Concord2007 <- subset(Concord2007, NAICS2002 %in% Occ_Con$NAICS)

Concord2012 <- Concord2012[,c(2,1)]
Concord2012 <- subset(Concord2012, NAICS2007 %in% Concord2007$NAICS2007)
#This looks like it has more than 978 unique NAICS codes, but many of them are duplicates
#length(unique(Concord2012$NAICS2012))

#join the Occ_Mat data to the Concordance
Occ_Con <- Concord2007 %>% left_join(Occ_Con, by=c("NAICS2002"="NAICS"))

#delete rows with old NAICS codes that no longer exist
Occ_Con <- Occ_Con[!is.na(Occ_Con$NAICS2007),]

#remove column with previous NAICS
Occ_Con$NAICS2002 <- NULL
colnames(Occ_Con)[1] <- "NAICS"

#average values where previous NAICS codes were combined
Occ_Con <- as.data.table(Occ_Con)
keys <- colnames(Occ_Con)[1]
Occ_Con <- Occ_Con[,lapply(.SD,mean),keys]



#join the Occ_Mat data to the Concordance
Occ_Con <- Concord2012 %>% left_join(Occ_Con, by=c("NAICS2007"="NAICS"))

#delete rows with old NAICS codes that no longer exist
Occ_Con <- Occ_Con[!is.na(Occ_Con$NAICS2012),]

#remove column with previous NAICS
Occ_Con$NAICS2007 <- NULL
colnames(Occ_Con)[1] <- "NAICS"

#average values where previous NAICS codes were combined
Occ_Con <- as.data.table(Occ_Con)
keys <- colnames(Occ_Con)[1]
Occ_Con <- Occ_Con[,lapply(.SD,mean),keys]


#Export

write.csv(Occ_Mat, "Occ/Occ_2006_NAICS2002.csv", row.names = F)
write.csv(Occ_Con, "Occ/Occ_2006_NAICS2012.csv", row.names = F)

#### Use Line 161 to skip the steps above
Occ_Con <- read.csv("Occ/Occ_2006_NAICS2012.csv", check.names = F, sep = ",", header = T, stringsAsFactors = F)

#Filter for traded
Traded <- read.csv("NAICS/traded.csv", header = T, sep = ",", stringsAsFactors = F)
Traded <- Traded[,c(1,7)]
Occ_Con <- Occ_Con %>% left_join(Traded, by="NAICS")
Occ_Con <- subset(Occ_Con, Traded_Local == "Traded")
Occ_Con$Traded_Local <- NULL

###### Create Correlation and Distance Matrices ###############################

# prepare matrix for Distance calculation
allIndustry <- Occ_Con$NAICS
Occ_Con$NAICS <- NULL
Occ_Con <- as.matrix(sapply(Occ_Con, as.numeric))

###############################################################################
# Euclidian Distance
###############################################################################

Dist_Occ <- as.matrix(dist(Occ_Con, method = "euclidean"))
rownames(Dist_Occ) <- allIndustry 
colnames(Dist_Occ) <- allIndustry


###############################################################################
# Correlation matrix - transpose b/c Cor measures columns (Dist measures rows)
###############################################################################

Cor_Occ <- t(Occ_Con)
Cor_Occ <- as.matrix(cor(Cor_Occ, use = "complete.obs"))
Cor_Occ <- 0.5*(1+Cor_Occ)
rownames(Cor_Occ) <- allIndustry 
colnames(Cor_Occ) <- allIndustry


##### Create Average Distance #################################################

##Code for first year/iteration
#AvgDist_Occ <- melt(Dist_Occ)
#colnames(AvgDist_Occ) <- c("Industry1", "Industry2", "2010")

#AvgCor_Occ <- melt(Cor_Occ)
#colnames(AvgCor_Occ) <- c("Industry1", "Industry2", "2010")

##Code for subsequent years/iterations
NextDist <- melt(Dist_Occ)
colnames(NextDist) <- c("Industry1", "Industry2", "2006")
NextCor <- melt(Cor_Occ)
colnames(NextCor) <- c("Industry1", "Industry2", "2006")

AvgDist_Occ <- AvgDist_Occ %>% left_join(NextDist, by=c("Industry1", "Industry2"))
AvgCor_Occ <- AvgCor_Occ %>% left_join(NextCor, by=c("Industry1", "Industry2"))

##Create averages
AvgDist_Occ$Avg <- rowMeans(AvgDist_Occ[,c(3:7)], na.rm = T)
AvgCor_Occ$Avg <- rowMeans(AvgCor_Occ[,c(3:7)], na.rm = T)

AvgDist_Occ <- AvgDist_Occ[,c(1,2,8)]
AvgCor_Occ <- AvgCor_Occ[,c(1,2,8)]

AvgDist_Occ_DF <- spread(AvgDist_Occ, Industry1, Avg, fill = 0)
AvgCor_Occ_DF <- spread(AvgCor_Occ, Industry1, Avg, fill = 0)

row.names(AvgDist_Occ_DF) <- AvgDist_Occ_DF$Industry2
row.names(AvgCor_Occ_DF) <- AvgCor_Occ_DF$Industry2

AvgDist_Occ_DF$Industry2 <- NULL
AvgCor_Occ_DF$Industry2 <- NULL

#Export

write.csv(AvgDist_Occ_DF, "Matrices/AvgDist_Occ.csv")
write.csv(AvgCor_Occ_DF, "Matrices/AvgCor_Occ.csv")
