library(dplyr)
library(tidyr)

# load historical data
Employees <- read.csv("County/Historical_Emp.csv", header = T, stringsAsFactors = F, sep = ",", check.names = F)
Establishments <- read.csv("County/Historical_Est.csv", header = T, stringsAsFactors = F, sep = ",", check.names = F)

# load concordances, based upon: https://www.census.gov/eos/www/naics/concordances/concordances.html ######
# I created custom files, because I had to wrangle the Census files ###
Concord2002 <- read.csv("NAICS/Concord_1998to2002.csv", header = T, sep = ",")
Concord2007 <- read.csv("NAICS/Concord_2002to2007.csv", header = T, sep = ",")
Concord2012 <- read.csv("NAICS/Concord_2007to2012.csv", header = T, sep = ",")
NAICS2012 <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", check.names = F)

#### Years 1998 to 2002 ######################################################

# filter concordances for the years 1998 to 2002 (last 4 columns)
Emp_to02 <- Employees[,c(1,2,(ncol(Employees)-4):ncol(Employees))]
Est_to02 <- Establishments[,c(1,2,(ncol(Establishments)-4):ncol(Establishments))]

# apply concordance for NAICS1998 to NAICS2002
Emp_to02 <- Emp_to02 %>% left_join(Concord2002, by=c("naics"="NAICS1997"))
Est_to02 <- Est_to02 %>% left_join(Concord2002, by=c("naics"="NAICS1997"))

# remove rows with NA in NAICS2002
Emp_to02 <- Emp_to02[!is.na(Emp_to02$NAICS2002),]
Est_to02 <- Est_to02[!is.na(Est_to02$NAICS2002),]

# Re-organize to get rid of old NAICS code
Emp_to02 <- Emp_to02[,c(1,8,3:7)]
Est_to02 <- Est_to02[,c(1,8,3:7)]

# regroup
Emp_to02 <- Emp_to02 %>% group_by(County, NAICS2002) %>% summarise_all(funs(sum(., na.rm = TRUE)))
Est_to02 <- Est_to02 %>% group_by(County,NAICS2002) %>% summarise_all(funs(sum(., na.rm = TRUE)))



#### Years 1998 to 2007 ######################################################

Emp_to07 <- Employees[,c(1,2,(ncol(Employees)-9):(ncol(Employees)-5))] %>% full_join(Emp_to02, by=c("County"="County", "naics"="NAICS2002"))
Est_to07 <- Establishments[,c(1,2,(ncol(Establishments)-9):(ncol(Establishments)-5))] %>% full_join(Est_to02, by=c("County"="County", "naics"="NAICS2002"))

Emp_to07 <- Emp_to07 %>% left_join(Concord2007, by=c("naics"="NAICS2002"))
Est_to07 <- Est_to07 %>% left_join(Concord2007, by=c("naics"="NAICS2002"))

#Remove NAICS row if that code no longer exists in current-NAICS
Emp_to07 <- Emp_to07[!is.na(Emp_to07$NAICS2007),]
Est_to07 <- Est_to07[!is.na(Est_to07$NAICS2007),]

# Re-organize to get rid of previous NAICS code
Emp_to07 <- Emp_to07[,c(1,13,3:12)]
Est_to07 <- Est_to07[,c(1,13,3:12)]

# regroup
Emp_to07 <- Emp_to07 %>% group_by(County,NAICS2007) %>% summarise_all(funs(sum(., na.rm = TRUE)))
Est_to07 <- Est_to07 %>% group_by(County,NAICS2007) %>% summarise_all(funs(sum(., na.rm = TRUE)))


 
#### Years 1998 to 2012 ######################################################

Emp_to12 <- Employees[,c(1,2,(ncol(Employees)-14):(ncol(Employees)-10))] %>% full_join(Emp_to07, by=c("County"="County", "naics"="NAICS2007"))
Est_to12 <- Establishments[,c(1,2,(ncol(Establishments)-14):(ncol(Establishments)-10))] %>% full_join(Est_to07, by=c("County"="County", "naics"="NAICS2007"))

Emp_to12 <- Emp_to12 %>% left_join(Concord2012, by=c("naics"="NAICS2007"))
Est_to12 <- Est_to12 %>% left_join(Concord2012, by=c("naics"="NAICS2007"))

Emp_to12 <- Emp_to12[!is.na(Emp_to12$NAICS2012),]
Est_to12 <- Est_to12[!is.na(Est_to12$NAICS2012),]

Emp_to12 <- Emp_to12[,c(1,18,3:17)]
Est_to12 <- Est_to12[,c(1,18,3:17)]

Emp_to12 <- Emp_to12 %>% group_by(County,NAICS2012) %>% summarise_all(funs(sum(., na.rm = TRUE)))
Est_to12 <- Est_to12 %>% group_by(County,NAICS2012) %>% summarise_all(funs(sum(., na.rm = TRUE)))



#### Years 1998 to 2017 #######################################################

Emp_to17 <- Employees[,c(1,2,(ncol(Employees)-19):(ncol(Employees)-15))] %>% full_join(Emp_to12, by=c("County"="County", "naics"="NAICS2012"))
Est_to17 <- Establishments[,c(1,2,(ncol(Establishments)-19):(ncol(Establishments)-15))] %>% full_join(Est_to12, by=c("County"="County", "naics"="NAICS2012"))

#Sort
Emp_to17 <- Emp_to17[with(Emp_to17, order(County, naics)),]
Est_to17 <- Est_to17[with(Est_to17, order(County, naics)),]
row.names(Emp_to17) <- NULL
row.names(Est_to17) <- NULL

#Filter out the non-NAICS2012
Emp_concord <- subset(Emp_to17, naics %in% NAICS2012$`NAICS 2012`)
Est_concord <- subset(Est_to17, naics %in% NAICS2012$`NAICS 2012`)



#### Export ##################################################################
write.csv(Emp_concord, "County/Concord_Emp.csv", row.names = F)
write.csv(Est_concord, "County/Concord_Est.csv", row.names = F)
