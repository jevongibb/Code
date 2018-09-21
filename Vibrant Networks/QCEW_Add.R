library(dplyr)

#Fix the error you created in Concords by updating Historical, then running Concord script

Historical_Emp <- read.csv("County/Imputed_Hist.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
Historical_Est <- read.csv("County/Establishments_Hist.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

Concord_Emp <- read.csv("County/Imputed_Concord.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
Concord_Est <- read.csv("County/Establishments_Concord.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)


### BLS  Employment data #########################################################################
Employees <- read.csv("2017.annual.singlefile.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

# Remove national-level data
Employees <- subset(Employees, area_fips != "01000")

# Filter to only Private Sector employment (prior CBP data is only Private & including State and Federal requires aggregating and complicates imputing from previous years)
Employees <- subset(Employees, own_code == 5)

# Filter to 6-digit NAICS
Employees <- Employees[nchar(Employees$industry_code) == 6,]

# Remove unnecessary columns for memory purposes
Employees <- Employees[,c(1,3,10,9,8)]
colnames(Employees) <- c("County", "naics", "Employees", "Establishments", "DataSuppression")
rownames(Employees) <- NULL

#Convert County column to integer and remove non-county rows
Employees$County <- as.integer(Employees$County) #NAs are for aggregated units (MSAs, etc)
Employees <- Employees[!is.na(Employees$County),]


#Join with CBP
Employees$naics <- as.integer(Employees$naics)
Historical_Emp <- Historical_Emp %>% full_join(Employees[,c(1,2,3,5)], by=c("County", "naics")) #Note the FULL join

#Impute employee number if data suppression (use previous year number from CBP data)
Historical_Emp$Employees <- ifelse(Historical_Emp$DataSuppression == "N", Historical_Emp$`2016`, Historical_Emp$Employees) 

#If no employees in previous year, then insert small number
Historical_Emp$Employees <- ifelse(Historical_Emp$DataSuppression == "N" & is.na(Historical_Emp$`2016`), 5, Historical_Emp$Employees)

#Re-arrange
Historical_Emp <- Historical_Emp[,1:22]
colnames(Historical_Emp)[22] <- 2017
Historical_Emp <- Historical_Emp[,c(1:2,22,3:21)]


### BLS  Establishment data #########################################################################

Establishments <- Employees[,c(1,2,4)]
Historical_Est <- Historical_Est %>% full_join(Establishments, by=c("County", "naics")) #Note the FULL join

#Re-arrange
colnames(Historical_Est)[22] <- 2017
Historical_Est <- Historical_Est[,c(1:2,22,3:21)]


### Add to Historical data ###################################################################

#I did this above, because I broke the Concorded data and had to re-concord

### write ####################################################################################
write.csv(Historical_Emp, "County/Historical_Emp.csv", row.names = F)
write.csv(Historical_Est, "County/Historical_Est.csv", row.names = F)

write.csv(Concord_Emp, "County/Imputed_Concord.csv", row.names = F)
write.csv(Concord_Est, "County/Establishments_Concord.csv", row.names = F)






### Old code #########################################################################

#Join historical
Historical_Emp <- Historical_Emp %>% left_join(CY_Imputed, by=c("County"="County", "naics"="naics"))
Historical_Emp <- Historical_Emp[,c(1:2,21,3:20)]

Historical_Est <- Historical_Est %>% left_join(CY_Est, by=c("County"="County", "naics"="naics"))
Historical_Est <- Historical_Est[,c(1:2,21,3:20)]

#Join concorded
Concord_Emp <- Concord_Emp %>% left_join(CY_Imputed, by=c("County"="County", "naics"="naics"))
Concord_Emp <- Concord_Emp[,c(1:2,21,3:20)]

Concord_Est <- Concord_Est %>% left_join(CY_Est, by=c("County"="County", "naics"="naics"))
Concord_Est <- Concord_Est[,c(1:2,21,3:20)]

#write
write.csv(Historical_Emp, "County/Imputed_Hist.csv", row.names = F)
write.csv(Historical_Est, "County/Establishments_Hist.csv", row.names = F)

write.csv(Concord_Emp, "County/Imputed_Concord.csv", row.names = F)
write.csv(Concord_Est, "County/Establishments_Concord.csv", row.names = F)