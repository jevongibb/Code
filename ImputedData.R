# Set the Year.
Year <- as.character("2015")
Yr <- Yr <- substr(Year, 3, 4)



# turn off scientific notation
options(scipen=999)

# load dplyr and tidyr package for data manipulation functionality (matrix calcs)
library(dplyr)
library(tidyr)

# set working directory
setwd("C:/Users/Jevon/Desktop/DevGPS/Data")

##In case R crashed and need to reload Imputed and Establishments
#Imputed <- read.csv("County/Imputed_Hist.csv", sep = ",", stringsAsFactors = F, header = T, check.names = F)
#Establishments <- read.csv("County/Establishments_Hist.csv", sep = ",", stringsAsFactors = F, header = T, check.names = F)
#Imputed$naics <- as.character(Imputed$naics)
#Establishments$naics <- as.character(Establishments$naics)

#####read input data ## available at https://www.census.gov/programs-surveys/cbp/data/datasets.html #########################
## Warning: 1999 data has duplicate rows (14191 and 14192) that can cause errors. Filter duplicates before using that data ##

Initial_data <- read.table(paste("County/cbp", Yr, "co.txt", sep = ""), header=TRUE, sep=",", stringsAsFactors=FALSE)
Regions <- read.csv("CZ/CommutingZones.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
Industries <- read.csv("NAICS/NAICS.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
EmpFlag <- read.csv("County/EmpFlag.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
NoiseFlag <- read.csv("County/NoiseFlag.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)
EntCodeRang <- read.csv("County/EntCodeRange.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F)

#Make extra columns for years before 2007
if(Year<2007) {
  Initial_data$emp_nf <- "D"
  Initial_data$qp1_nf <- "D"
  Initial_data$ap_nf <- "D"
  Initial_data <- Initial_data[,c(1:4,24,5,25,6,26,7:23)]
} 


#Pull relevant data from initial data
Data <- Initial_data[,c(1:24)]
Data <- Data[,-c(7:10,20)]

#Replace State and City codes with a single County code
Data <- subset(Data, Data[,2] != 999)
Data$County <- Data[,1]*1000+Data[,2]
Data <- Data[,c(20,3:6,8:19,7)]


#####Build the imputed data set################################################

#Pull the given Employee values (EMP) that do not have Employment Flags (EMPFLAG)
Data$NewEmp <- ifelse(Data[,3]=="", Data[,5], 0)

#If no EMPFLAG, then Min = Emp * (1- NoiseFlag range), else Minimum EMPFLAG value
Data$Min <- ifelse(Data[,3]=="", 
                   Data$NewEmp * (1-NoiseFlag$Max[match(Data[,4], NoiseFlag$Code)]),
                   EmpFlag$Min[match(Data[,3], EmpFlag$Code)])

#If no EMPFLAG, then Max = Emp * (1+ NoiseFlag range), else Maximum EMPFLAG value
Data$Max <- ifelse(Data[,3]=="", 
                   as.numeric(Data$NewEmp) * (1+NoiseFlag$Max[match(Data[,4], NoiseFlag$Code)]),
                   EmpFlag$Max[match(Data[,3], EmpFlag$Code)])

#If no EMPFLAG, then Max = Max, else Max is matrix multiplication of enterprises * enterprise size data
Data$Max2 <- ifelse(Data[,3]=="", 
                      Data$Max,
                      as.matrix(Data[, 6:17]) %*% as.matrix(EntCodeRang$Max))

#Max = minimum of Max and Max2
Data$Max3 <- with(Data, pmin(Data$Max, Data$Max2))

#Generate a second estimate of employees using middle of Min and Max3
Data$SecondEst <- (Data$Min+Data$Max3)/2

#Re-format NAICS codes (substitute "/" for "-") because it makes life easier
Data$Replace <- paste0(gsub("-", "/", Data[,2]), "/")

#Create a column to identify the digit-level of the NAICS codes (ie: 3-digit NAICS vs. 6-digit)
Data$Digits <- sapply(gregexpr("/", Data$Replace, fixed=TRUE), function(x) { x[[1]][1]-1 })

#Generate digit-level NAICS codes
Data$NAICS5 <- substr(Data$Replace, 1, 5)
Data$NAICS4 <- substr(Data$Replace, 1, 4)
Data$NAICS3 <- substr(Data$Replace, 1, 3)
Data$NAICS2 <- substr(Data$Replace, 1, 2)

#Pull SecondEst for 6-digit NAICS codes
Data$d6 <- ifelse(Data$Digits==6, Data$SecondEst, 0)



####Fixing for 5-digit###########################################################

#Generate sum of 6-digit NAICS
d5_sums <- Data %>% filter(Digits==6) %>% group_by(County, NAICS5) %>% summarise(total=sum(d6))

#Join those sums to the 6-digit and 5-digit Data entries
Data <- Data %>% left_join(d5_sums, by=c("County"="County", "NAICS5"="NAICS5"))
Data$total[is.na(Data$total)] <- 0

#Test whether the new sum of 6-digit NAICS is within the Min and Max, if not create value to adjust by
d5_fix <- Data[Data$Digits==5,]
d5_fix$fix <- ifelse(is.na(d5_fix$total),
                   0,
                   ifelse(d5_fix$total<d5_fix$Min,
                          d5_fix$Min/d5_fix$total,
                          ifelse(d5_fix$total>d5_fix$Max3,
                                 d5_fix$Max3/d5_fix$total,
                                 1)))
d5_fix <- d5_fix[, c("County", "NAICS5", "fix")]

#Generate warnings if multiplier is greater than 2
warning_list <- data.frame(County=numeric(), NAICS=numeric(), fix=numeric())
warnings <- subset(d5_fix, d5_fix$fix>2)
colnames(warnings)[2] <- "NAICS"
warning_list <- rbind(warning_list, warnings)
row.names(warning_list) <- NULL

#Add d5_fix column to Data
Data <- Data %>% left_join(d5_fix, by=c("County"="County", "NAICS5"="NAICS5"))
Data$fix[is.na(Data$fix)] <- 1 ## Make sure there is always a fix
colnames(Data)[ncol(Data)] <- "d5_fix"

#Adjust employee numbers by d5_fix, if d5_fix is less than 2. Else, do not adjust.
Data$total <- ifelse(Data$Digits==5, Data$total, 0) ## Makes the math easier
Data$d5 <- ifelse(Data$d5_fix<2, Data$d6 * Data$d5_fix + Data$total * Data$d5_fix, Data$d6 + Data$total)
Data$d5[is.na(Data$d5)] <- 0 ## Makes the math work later on. Will turn back to NA at the end.

#Remove columns created for purpose of calculation
Data$total <- NULL



####fixing for 4-digit (Rinse and repeat steps from 5-digit)####################

d4_sums <- Data %>% filter(Digits==6) %>% group_by(County, NAICS4) %>% summarise(total=sum(d5))
Data <- Data %>% left_join(d4_sums, by=c("County"="County", "NAICS4"="NAICS4"))
Data$total[is.na(Data$total)] <- 0

d4_fix <- Data[Data$Digits==4,]
d4_fix$fix <- ifelse(is.na(d4_fix$total),
                     0,
                     ifelse(d4_fix$total<d4_fix$Min,
                            d4_fix$Min/d4_fix$total,
                            ifelse(d4_fix$total>d4_fix$Max3,
                                   d4_fix$Max3/d4_fix$total,
                                   1)))
d4_fix <- d4_fix[, c("County", "NAICS4", "fix")]

warnings <- subset(d4_fix, d4_fix$fix>2)
colnames(warnings)[2] <- "NAICS"
warning_list <- rbind(warning_list, warnings)
row.names(warning_list) <- NULL

Data <- Data %>% left_join(d4_fix, by=c("County"="County", "NAICS4"="NAICS4"))
Data$fix[is.na(Data$fix)] <- 1
colnames(Data)[ncol(Data)] <- "d4_fix"

Data$total <- ifelse(Data$Digits==4, Data$total, 0) ## Makes the math easier
Data$d4 <- ifelse(Data$d4_fix<2, Data$d5 * Data$d4_fix + Data$total * Data$d4_fix, Data$d5 + Data$total)
Data$d4[is.na(Data$d4)] <- 0

Data$total <- NULL



####fixing for 3-digit#####################################

d3_sums <- Data %>% filter(Digits==6) %>% group_by(County, NAICS3) %>% summarise(total=sum(d4))
Data <- Data %>% left_join(d3_sums, by=c("County"="County", "NAICS3"="NAICS3"))
Data$total[is.na(Data$total)] <- 0

d3_fix <- Data[Data$Digits==3,]
d3_fix$fix <- ifelse(is.na(d3_fix$total),
                     0,
                     ifelse(d3_fix$total<d3_fix$Min,
                            d3_fix$Min/d3_fix$total,
                            ifelse(d3_fix$total>d3_fix$Max3,
                                   d3_fix$Max3/d3_fix$total,
                                   1)))
d3_fix <- d3_fix[, c("County", "NAICS3", "fix")]

warnings <- subset(d3_fix, d3_fix$fix>2)
colnames(warnings)[2] <- "NAICS"
warning_list <- rbind(warning_list, warnings)
row.names(warning_list) <- NULL

Data <- Data %>% left_join(d3_fix, by=c("County"="County", "NAICS3"="NAICS3"))
Data$fix[is.na(Data$fix)] <- 1
colnames(Data)[ncol(Data)] <- "d3_fix"

Data$total <- ifelse(Data$Digits==3, Data$total, 0) ## Makes the math easier
Data$d3 <- ifelse(Data$d3_fix<2, Data$d4 * Data$d3_fix + Data$total * Data$d3_fix, Data$d4 + Data$total)
Data$d3[is.na(Data$d3)] <- 0

Data$total <- NULL



#####fixing for 2-digit#####################################

d2_sums <- Data %>% filter(Digits==6) %>% group_by(County, NAICS2) %>% summarise(total=sum(d3))
Data <- Data %>% left_join(d2_sums, by=c("County"="County", "NAICS2"="NAICS2"))
Data$total[is.na(Data$total)] <- 0

d2_fix <- Data[Data$Digits==2,]
d2_fix$fix <- ifelse(is.na(d2_fix$total),
                     0,
                     ifelse(d2_fix$total<d2_fix$Min,
                            d2_fix$Min/d2_fix$total,
                            ifelse(d2_fix$total>d2_fix$Max3,
                                   d2_fix$Max3/d2_fix$total,
                                   1)))
d2_fix <- d2_fix[, c("County", "NAICS2", "fix")]

warnings <- subset(d2_fix, d2_fix$fix>2)
colnames(warnings)[2] <- "NAICS"
warning_list <- rbind(warning_list, warnings)
row.names(warning_list) <- NULL

Data <- Data %>% left_join(d2_fix, by=c("County"="County", "NAICS2"="NAICS2"))
Data$fix[is.na(Data$fix)] <- 1
colnames(Data)[ncol(Data)] <- "d2_fix"


Data$total <- ifelse(Data$Digits==2, Data$total, 0) ## Makes the math easier
Data$d2 <- ifelse(Data$d2_fix<2, Data$d3 * Data$d2_fix + Data$total * Data$d2_fix, Data$d3 + Data$total)

Data$total <- NULL

##adjusting final result
#Make sure new estimate is less than Max3 and more than the Min. Excluded enterprises may have caused over-adjustment above.
Data$Imputed <- pmax(pmin(Data$d2, Data$Max3), Data$Min)

#Round the Final_Est
Data$Imputed <- round(Data$Imputed+0.0000000001)   # because R converts 0.5 to 0

#Replace zeros with NA
Data[,31:39][Data[,31:39] == 0] <- NA

#removing unnecessary columns
Data <- Data[, c(1, 2, 5, 24, 31:40, 18)]


#Test for duplicates
sum(duplicated(Data))
#Data <- Data[!duplicated(Data)]

### Exporting and Combining Data #################################################

#export data 
write.csv(warning_list, paste("County/Warnings/", Year, "_warnings.csv", sep = ""), row.names = F)
write.csv(Data, paste("County/", Year, "_imputed.csv", sep = ""), row.names = F)

#filter to only 6-digit NAICS
Data2 <- Data[ grep("/", Data[,2], invert = TRUE),]
Data2 <- Data2[ grep("-", Data2[,2], invert = TRUE),]
rownames(Data2) <- NULL



##To create a multi-yr table, for the first iteration, use this section. ########

## Create table for imputed number of employees
#Imputed <- Data2[,c(1, 2, 14)]
#colnames(Imputed)[2:3] <- c("naics", Year)

## Create table for number of establishments
#Establishments <- Data2[,c(1,2,15)]
#colnames(Establishments)[2:3] <- c("naics", Year)

#################################################################################

##Add current year to multi-yr table ############################################
#After first iteration, turn the above section into a comment and use this section.

#add current year to imputed data set
CY_Imputed <- Data2[,c(1, 2, 14)]
colnames(CY_Imputed)[3] <- Year
Imputed <- Imputed %>% full_join(CY_Imputed, by=c("County"="County", "naics"="naics")) ## CAPS issue for 2015

#add current year to establishments data set
CY_Est <- Data2[,c(1,2,15)]
colnames(CY_Est)[3] <- Year
Establishments <- Establishments %>% full_join(CY_Est, by=c("County"="County", "naics"="naics")) ## CAPS issue for 2015

#re-order the data sets, b/c County/naics combos get added to the bottom due to full_join
Imputed <- Imputed[with(Imputed, order(County, naics)),]
Establishments <- Establishments[with(Establishments, order(County, naics)),]
row.names(Imputed) <- NULL
row.names(Establishments) <- NULL



sum(duplicated(Imputed))
sum(duplicated(Establishments))

## Export Imputed and Establishments each cycle, in case R crashes
write.csv(Imputed, "County/Imputed_Hist.csv", row.names = F)
write.csv(Establishments, "County/Establishments_Hist.csv", row.names = F)
