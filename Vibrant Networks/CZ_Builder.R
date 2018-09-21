Employees <- read.csv("County/Concord_Emp.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
Establishments <- read.csv("County/Concord_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

### Convert Counties to CZs ####################################################
#Note: You will lose some counties in Alaska (2105  2195  2198  2230  2275)

#load CZ definition file
CZdef <- read.csv("CZ/CommutingZones.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
CZdef <- CZdef[,1:2]
colnames(CZdef)[2] <- "CZ"

#Combine Employees and CZDef, reorganize columns, then aggregate by CZ
CZ_Employees <- inner_join(Employees, CZdef, by = c("County" = "FIPS"))
CZ_Employees <- CZ_Employees[,c(length(CZ_Employees),2:(length(CZ_Employees)-1))]
CZ_Employees <- CZ_Employees %>% group_by(CZ, naics) %>% summarise_all(funs(sum(., na.rm = TRUE)))

#Same for Establishments
CZ_Establishments <- inner_join(Establishments, CZdef, by = c("County" = "FIPS"))
CZ_Establishments <- CZ_Establishments[,c(length(CZ_Establishments),2:(length(CZ_Establishments)-1))]
CZ_Establishments <- CZ_Establishments %>% group_by(CZ, naics) %>% summarise_all(funs(sum(., na.rm = TRUE)))

#write CSV
write.csv(CZ_Employees, "CZ/Employees_Q.csv", row.names = F)
write.csv(CZ_Establishments, "CZ/Establishments_Q.csv", row.names = F)
