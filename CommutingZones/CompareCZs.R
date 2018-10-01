#Load data (Earnings data available at: https://apps.bea.gov/regional/downloadzip.cfm)
Earnings <- read.csv("EconData/CA5N_2001_2016__ALL_AREAS.csv", header = T, check.names = F, stringsAsFactors = F)
CZdef <- read.csv("CZ/CZs.csv", header = T, check.names = F, stringsAsFactors = F)


#Format Earnings
Earnings[,c(1,8:23)] <- sapply(Earnings[,c(1,8:23)], as.integer)
debug <- Earnings[!complete.cases(Earnings),] # Some NAs get introduced above, but not a problem

Earnings <- subset(Earnings, GeoFIPS>=1000) # Remove National Data
Population <- subset(Earnings, LineCode == 20)
Earnings <- subset(Earnings, LineCode == 30)

Earnings <- Earnings[,c(1,2,23)]
Population <- Population[,c(1,2,23)]

Earnings <- subset(Earnings, GeoFIPS %% 1000 != 0) # Remove non-County-level data
Population <- subset(Population, GeoFIPS %% 1000 != 0)

Earnings <- Earnings %>% left_join(Population[,c(1,3)], by="GeoFIPS")
colnames(Earnings)[3:4] <- c("IncomePC", "Population")


#Join with CZ definitions
Data <- CZdef %>% left_join(Earnings[,c(1,3,4)], by=c("county"="GeoFIPS"))

#Pull Population by CZ
CZpop <- Data %>% group_by(group) %>% summarise(CZpop=sum(Population))

#Join pop to data
Data2 <- Data %>% left_join(CZpop, by="group")
Data2$Percent <- Data2$Population / Data2$CZpop
Data2$Calc <- Data2$IncomePC * Data2$Percent

#Pull IncomePC by CZ
CZinc <- Data2 %>% group_by(group) %>% summarise(CZinc=sum(Calc))

#Join CZinc to Data
Data <- Data %>% left_join(CZinc, by="group")
colnames(Data)[c(5,7)] <- c("CountyIncomePC", "CZIncomePC")

write.csv(Data, "CZ/CZwithIncome.csv", row.names = F)
