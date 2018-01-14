#Run DevGPS_Format_County.R
#Stop at Line 53

#Load the Imputed_Hist
Imputed <- read.csv("County/Imputed_Hist.csv", header = T, sep = ",", check.names = F, stringsAsFactors = F)
Imputed <- Imputed[,1:3]
Imputed <- Imputed[!is.na(Imputed$`2015`),]

Data <- subset(Data, Flag != "")
Test <- Data %>% left_join(Imputed, by=c("Region"="County", "Industry"="naics"))
Test$Diff <- abs(Test$Quantity - Test$`2015`)
Test$Percent <- Test$Diff/Test$Quantity
Deviation <- mean(Test$Percent, na.rm = T)

#For 2015, Deviation was 0.4817761
