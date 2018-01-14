library(dplyr)
library(tidyr)
library(reshape2)

options(scipen=999)


#load data (available at https://www.bea.gov/industry/io_annual.htm)
IO_Make <- read.csv("IO/IOMake_Data.csv", header = T, stringsAsFactors = F, check.names = F, sep = ",", skip = 4)
IO_Use <- read.csv("IO/IOUse_Data.csv", header = T, stringsAsFactors = F, check.names = F, sep = ",", skip = 4)
IO_NAICS <- read.csv("NAICS/2012.csv", header = T, stringsAsFactors = F, check.names = F, sep = ",")

#edit data to remove excess columns/rows
IO_Make <- IO_Make[-c(381:395),-c(382:392)]
IO_Use <- IO_Use[-c(380:398),-c(383:414)]

#remove 331314, b/c it causes a non-square matrix due to not being provided on both sides.
IO_Make <- IO_Make[-56,]
IO_Use$`331314` <- NULL

#remove commas in order to convert from character to numeric
IO_Make[,3:length(IO_Make)] <- apply(IO_Make[,3:length(IO_Make)], 2, function(x) gsub(",", "", x))
IO_Use[,3:length(IO_Use)] <- apply(IO_Use[,3:length(IO_Use)], 2, function(x) gsub(",", "", x))

#convert characters to numeric
IO_Make[,3:length(IO_Make)] <- apply(IO_Make[,3:length(IO_Make)], 2, function(x) as.numeric(x))
IO_Use[,3:length(IO_Use)] <- sapply(IO_Use[,3:length(IO_Use)], as.numeric)


#Create Make(i,j)
IO_Make2 <- melt(IO_Make, id=c("Code", "Industry Description"))
IO_Make2$`Industry Description`<- NULL
IO_Make2$variable <- as.character(IO_Make2$variable)

Make_Total <- IO_Make2 %>% group_by(Code) %>% summarise(Total=sum(value, na.rm=T))

IO_Make2 <- IO_Make2 %>% left_join(Make_Total, by=c("Code"="Code"))
IO_Make2$CtoV <- IO_Make2$value/IO_Make2$Total

IO_Make2 <- IO_Make2[,c(1,2,5)]
IO_Make3 <- IO_Make2
IO_Make3 <- IO_Make3 %>% left_join(IO_Make2, by=c("Code"="variable", "variable"="Code"))
colnames(IO_Make3) <- c("i", "j", "i_to_j", "j_to_i")

IO_Make3$Max <- apply(IO_Make3[, 3:4], 1, function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA))
IO_Make3 <- IO_Make3[,c(1,2,5)]
colnames(IO_Make3)[3] <- "Make"

#Create Use(i,j)
IO_Use2 <- melt(IO_Use, id=c("Code", "Commodity Description"))
IO_Use2$`Commodity Description`<- NULL
IO_Use2$variable <- as.character(IO_Use2$variable)

Use_Total <- IO_Use2 %>% group_by(Code) %>% summarise(Total=sum(value, na.rm=T))

IO_Use2 <- IO_Use2 %>% left_join(Use_Total, by=c("Code"="Code"))
IO_Use2$CtoV <- IO_Use2$value/IO_Use2$Total

IO_Use2 <- IO_Use2[,c(1,2,5)]
IO_Use3 <- IO_Use2
IO_Use3 <- IO_Use3 %>% left_join(IO_Use2, by=c("Code"="variable", "variable"="Code"))
colnames(IO_Use3) <- c("i", "j", "i_to_j", "j_to_i")

IO_Use3$Max <- apply(IO_Use3[, 3:4], 1, function(x) ifelse( !all(is.na(x)), max(x, na.rm = T), NA))
IO_Use3 <- IO_Use3[,c(1,2,5)]
colnames(IO_Use3)[3] <- "Use"

#Combine Make and Use
IO_Combo <- IO_Make3 %>% left_join(IO_Use3, by = c("i"="i", "j"="j"))
IO_Combo$Max <- apply(IO_Combo[, 3:4], 1, function(x) ifelse( !all(is.na(x)), max(x, na.rm = T), NA))
IO_Combo$Max[is.na(IO_Combo$Max)] <- 0

###build data frame to join Make and Use values to NAICS list (so that you create duplicates when value is a parent)

#empty naics matrix
naics <- read.csv("naics/2012.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
naics2 <- as.data.frame(matrix(NA, ncol = 978, nrow = 978))
colnames(naics2) <- naics$NAICS
naics <- cbind(naics, naics2)
naics <- melt(naics, id="NAICS")

#apply concord
concord <- read.csv("NAICS/IO_NAICS_Concord.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
naics <- naics %>% left_join(concord, by="NAICS")
naics$variable <- as.integer(as.character(naics$variable))
naics <- naics %>% left_join(concord, by=c("variable"="NAICS"))
colnames(naics)[4:5] <- c("i", "j")
naics$value <- NULL

#left join w/ IO Combo
IO_data <- naics %>% left_join(IO_Combo, by=c("i"="i", "j"="j"))
IO_data$i <- IO_data$j <- IO_data$Make <- IO_data$Use <- NULL
colnames(IO_data) <- c("Industry1", "Industry2", "Distance")

#IO_data <- read.csv("IO_data.csv", header = T, sep = ",", stringsAsFactors = F)
IO_data[is.na(IO_data)] <- 0

#Load Traded industries
Traded <- read.csv("NAICS/traded.csv", header = T, sep = ",", stringsAsFactors = F)
Traded <- Traded[,c(1,7)]

IO_data <- IO_data %>% left_join(Traded, by=c("Industry1"="NAICS"))
IO_data <- subset(IO_data, Traded_Local == "Traded")
IO_data$Traded_Local <- NULL

IO_data <- IO_data %>% left_join(Traded, by=c("Industry2"="NAICS"))
IO_data <- subset(IO_data, Traded_Local == "Traded")
IO_data$Traded_Local <- NULL


IO_data_DF <- spread(IO_data, Industry1, Distance, fill = 0)
rownames(IO_data_DF) <- IO_data_DF$Industry2
IO_data_DF$Industry2 <- NULL

write.csv(IO_data_DF, "Matrices/IO_data.csv")