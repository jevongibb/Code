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

IO_Make3[is.na(IO_Make3)] <- 0
IO_Make3$Max <- pmax(IO_Make3$i_to_j, IO_Make3$j_to_i)
IO_Make3$Mean <- rowMeans(IO_Make3[,3:4])

IO_Make3 <- IO_Make3[,c(1,2,5,6)]
colnames(IO_Make3)[c(3,4)] <- c("Make_Max", "Make_Mean")

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

IO_Use3[is.na(IO_Use3)] <- 0
IO_Use3$Max <- pmax(IO_Use3$i_to_j, IO_Use3$j_to_i)
IO_Use3$Mean <- rowMeans(IO_Use3[,3:4])

IO_Use3 <- IO_Use3[,c(1,2,5,6)]
colnames(IO_Use3)[c(3,4)] <- c("Use_Max", "Use_Mean")

#Combine Make and Use
IO_Combo <- IO_Make3 %>% left_join(IO_Use3, by = c("i"="i", "j"="j"))
IO_Combo$Max <- pmax(IO_Combo$Make_Max, IO_Combo$Use_Max)
IO_Combo$Mean <- rowMeans(IO_Combo[,c(4,6)])


###build data frame to join Make and Use values to NAICS list (so that you create duplicates when value is a parent)

#empty naics matrix
naics <- read.csv("naics/2012.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
naics2 <- as.data.frame(matrix(NA, ncol = 978, nrow = 978))
colnames(naics2) <- naics$NAICS
naics <- cbind(naics, naics2)
naics <- melt(naics, id="NAICS")

#apply concord (This is different than Yr-Yr Concord from other scripts)
concord <- read.csv("NAICS/IO_NAICS_Concord.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
naics <- naics %>% left_join(concord, by="NAICS")
naics$variable <- as.integer(as.character(naics$variable))
naics <- naics %>% left_join(concord, by=c("variable"="NAICS"))
colnames(naics)[4:5] <- c("i", "j")
naics$value <- NULL

### Test the difference between Max and Mean matrices (cor), maybe also test with dist

#Create IO_data_Max
#left join w/ IO Combo
IO_data <- naics %>% left_join(IO_Combo[,c(1,2,7)], by=c("i", "j"))
IO_data <- IO_data[,c(1,2,5)]
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

IO_data_Max <- IO_data_DF

#Create IO_data_Mean
IO_data <- naics %>% left_join(IO_Combo[,c(1,2,8)], by=c("i", "j"))
IO_data <- IO_data[,c(1,2,5)]
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

IO_data_Max <- IO_data_DF

#Test correlation between Max and Mean (Prior output: .9857442)
cor(c(as.matrix(IO_data_Max)), c(as.matrix(IO_data_Mean)))

write.csv(IO_data_Max, "Matrices/IO_data_Max.csv")

IO_data <- as.matrix(IO_data_Max)
IO_dist <- as.matrix(dist(IO_data, method = "euclidean"))

cor(c(as.matrix(IO_data)), c(as.matrix(IO_dist)))
#Correlation was -0.1152748

write.csv(IO_dist, "Matrices/IO_data_dist.csv")
