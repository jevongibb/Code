library(dplyr)
library(tidyr)

# load historical data
Imputed <- read.csv("County/Imputed_Hist.csv", header = T, stringsAsFactors = F, sep = ",", check.names = F)
Establishments <- read.csv("County/Establishments_Hist.csv", header = T, stringsAsFactors = F, sep = ",", check.names = F)

# load concordances, based upon: https://www.census.gov/eos/www/naics/concordances/concordances.html ######
# I created custom files, because I did not like (could not use)the Census files ###
Concord2002 <- read.csv("NAICS/Concord_1998to2002.csv", header = T, sep = ",")
Concord2007 <- read.csv("NAICS/Concord_2002to2007.csv", header = T, sep = ",")
Concord2012 <- read.csv("NAICS/Concord_2007to2012.csv", header = T, sep = ",")
NAICS2012 <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", check.names = F)

#### Years 1998 to 2002 ######################################################

# filter concordances for the years 1998 to 2002
Imputed_to02 <- Imputed[,c(1,2,16:20)]
Est_to02 <- Establishments[,c(1,2,16:20)]

# apply concordance for NAICS1998 to NAICS2002
Imputed_to02 <- Imputed_to02 %>% left_join(Concord2002, by=c("naics"="NAICS1997"))
Est_to02 <- Est_to02 %>% left_join(Concord2002, by=c("naics"="NAICS1997"))

# remove rows with NA in NAICS2002
Imputed_to02 <- Imputed_to02[!is.na(Imputed_to02$NAICS2002),]
Est_to02 <- Est_to02[!is.na(Est_to02$NAICS2002),]

# Re-organize to get rid of old NAICS code
Imputed_to02 <- Imputed_to02[,c(1,8,3:7)]
Est_to02 <- Est_to02[,c(1,8,3:7)]

# regroup
Imputed_to02 <- Imputed_to02 %>% group_by(County, NAICS2002) %>% summarise_all(funs(sum(., na.rm = TRUE)))
Est_to02 <- Est_to02 %>% group_by(County,NAICS2002) %>% summarise_all(funs(sum(., na.rm = TRUE)))



#### Years 1998 to 2007 ######################################################

Imputed_to07 <- Imputed[,c(1,2,11:15)] %>% full_join(Imputed_to02, by=c("County"="County", "naics"="NAICS2002"))
Est_to07 <- Establishments[,c(1,2,11:15)] %>% full_join(Est_to02, by=c("County"="County", "naics"="NAICS2002"))

Imputed_to07 <- Imputed_to07 %>% left_join(Concord2007, by=c("naics"="NAICS2002"))
Est_to07 <- Est_to07 %>% left_join(Concord2007, by=c("naics"="NAICS2002"))

#Remove NAICS row if that code no longer exists in current-NAICS
Imputed_to07 <- Imputed_to07[!is.na(Imputed_to07$NAICS2007),]
Est_to07 <- Est_to07[!is.na(Est_to07$NAICS2007),]

# Re-organize to get rid of previous NAICS code
Imputed_to07 <- Imputed_to07[,c(1,13,3:12)]
Est_to07 <- Est_to07[,c(1,13,3:12)]

# regroup
Imputed_to07 <- Imputed_to07 %>% group_by(County,NAICS2007) %>% summarise_all(funs(sum(., na.rm = TRUE)))
Est_to07 <- Est_to07 %>% group_by(County,NAICS2007) %>% summarise_all(funs(sum(., na.rm = TRUE)))


 
#### Years 1998 to 2012 ######################################################

Imputed_to12 <- Imputed[,c(1,2,6:10)] %>% full_join(Imputed_to07, by=c("County"="County", "naics"="NAICS2007"))
Est_to12 <- Establishments[,c(1,2,6:10)] %>% full_join(Est_to07, by=c("County"="County", "naics"="NAICS2007"))

Imputed_to12 <- Imputed_to12 %>% left_join(Concord2012, by=c("naics"="NAICS2007"))
Est_to12 <- Est_to12 %>% left_join(Concord2012, by=c("naics"="NAICS2007"))

Imputed_to12 <- Imputed_to12[!is.na(Imputed_to12$NAICS2012),]
Est_to12 <- Est_to12[!is.na(Est_to12$NAICS2012),]

Imputed_to12 <- Imputed_to12[,c(1,18,3:17)]
Est_to12 <- Est_to12[,c(1,18,3:17)]

Imputed_to12 <- Imputed_to12 %>% group_by(County,NAICS2012) %>% summarise_all(funs(sum(., na.rm = TRUE)))
Est_to12 <- Est_to12 %>% group_by(County,NAICS2012) %>% summarise_all(funs(sum(., na.rm = TRUE)))



#### Years 1998 to 2015 #######################################################

Imputed_to15 <- Imputed[,c(1,2,3:5)] %>% full_join(Imputed_to12, by=c("County"="County", "naics"="NAICS2012"))
Est_to15 <- Establishments[,c(1,2,3:5)] %>% full_join(Est_to12, by=c("County"="County", "naics"="NAICS2012"))

#Sort
Imputed_to15 <- Imputed_to15[with(Imputed_to15, order(County, naics)),]
Est_to15 <- Est_to15[with(Est_to15, order(County, naics)),]
row.names(Imputed_to15) <- NULL
row.names(Est_to15) <- NULL

#Filter out the non-NAICS2012
Imputed_concord <- subset(Imputed_to15, naics %in% NAICS2012$`NAICS 2012`)
Est_concord <- subset(Est_to15, naics %in% NAICS2012$`NAICS 2012`)



#### Export ##################################################################
write.csv(Imputed_concord, "County/Imputed_Concord.csv", row.names = F)
write.csv(Est_concord, "County/Establishments_Concord.csv", row.names = F)
