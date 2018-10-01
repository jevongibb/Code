options(scipen=999)
library(dplyr)
library(tidyr)

#Load region definitions
CZdef <- read.csv("CZ/CommutingZones.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F)
MSAdef <- read.csv("MSA/MSAdef.csv", sep = ",", header = F, check.names = F, stringsAsFactors = F)
colnames(MSAdef) <- c("ID", "Name", "FIPS", "County")


#Choose region to run cohort analysis
Region <- 56

#If you don't know the ID, use the line of code below
#find <- CZdef[grep("Orleans", CZdef$MSA2003), ]

#Choose whether to group by County, MSA, or Commuting Zone (CZ)
RegionGroup <- "CZ"



### Economic Data ####################################################################

## Industry composition
#Load industry composition by employees for all counties in most recent year from CBP (result of custom algorithm)
#Raw data available at: https://www2.census.gov/programs-surveys/cbp/datasets/2016/cbp16msa.zip
#Algorithm available at: My github repository
IndustryComposition <- read.csv("County/Concord_Emp.csv", sep = ",", header = T, stringsAsFactors = F, check.names = F)

#Pull most recent year from CBP (not from BLS, which is more recent)
IndustryComposition <- IndustryComposition[,c(1,2,4)]
IndustryComposition <- IndustryComposition[!is.na(IndustryComposition$`2016`),]

#Spread the dataframe
IndustryComposition <- spread(IndustryComposition, naics, `2016`, fill = 0)


##Income, available from American FactFinder: INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS) 5-Year Estimates
Income <- read.csv("EconData/ACS_16_5YR_S1901_with_ann.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 1)

#Filter for Household Income by Range, Median, and Mean Income
Income <- Income[,c(2, 3, 12, 20, 28, 36, 44, 52, 60, 68, 76, 84, 92, 100)]

#Gini and Income Per Capita (I did not pull this, b/c Income captures the income structure)


##Labor data, available from American FactFinder: WORK STATUS IN THE PAST 12 MONTHS, 5-Year Estimates
Labor <- read.csv("EconData/ACS_16_5YR_S2301_with_ann.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 1)

#Filter for Unemployment and Prime-Age LFPR
Labor <- Labor[,c(2, 3, 10, 28, 30, 36, 38, 44, 46, 52, 54)]
Labor[3:11] <- sapply(Labor[3:11], as.numeric)
debug <- Labor[!complete.cases(Labor), ] # In case NAs introduced
Labor[is.na(Labor)] <- 0

Labor$`Prime Age LFPR` <- (Labor$`Total; Estimate; AGE - 25 to 29 years` * Labor$`Labor Force Participation Rate; Estimate; AGE - 25 to 29 years` +
  Labor$`Total; Estimate; AGE - 30 to 34 years` * Labor$`Labor Force Participation Rate; Estimate; AGE - 30 to 34 years` +
  Labor$`Total; Estimate; AGE - 35 to 44 years` * Labor$`Labor Force Participation Rate; Estimate; AGE - 35 to 44 years` +
  Labor$`Total; Estimate; AGE - 45 to 54 years` * Labor$`Labor Force Participation Rate; Estimate; AGE - 45 to 54 years`) /
  (Labor$`Total; Estimate; AGE - 25 to 29 years` +
  Labor$`Total; Estimate; AGE - 30 to 34 years` +
  Labor$`Total; Estimate; AGE - 35 to 44 years` +
  Labor$`Total; Estimate; AGE - 45 to 54 years`)

Labor <- Labor[,c(1:3, 12)]


## Productivity. Uses methodology from https://bea.gov/papers/pdf/BEA%20WP_Research%20Agenda%20for%20GDP%20County.pdf
# Earnings data available from American FactFinder, 5-yr Estimates
# GDP data available from BEA in 2017 current dollars, https://www.bea.gov/iTable/iTable.cfm?reqid=70&step=1&isuri=1&acrdn=2#reqid=70&step=1&isuri=1

#Load data
#EarningsByCounty <- read.csv("EconData/ACS_16_5YR_S2001_with_ann_Counties.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 1)
#EarningsByState <- read.csv("EconData/ACS_16_5YR_S2001_with_ann_States.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 1)
Earnings <- read.csv("EconData/CA5N_2001_2016__ALL_AREAS.csv", header = T, check.names = F, stringsAsFactors = F)
GDPbyState <- read.csv("EconData/GDP_byState.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 4)
HoursWorked <- read.csv("EconData/ACS_16_5YR_B23018_with_ann.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 1)

#Format Earnings
Earnings[,c(1,8:23)] <- sapply(Earnings[,c(1,8:23)], as.integer)
debug <- Earnings[!complete.cases(Earnings),] # Some NAs get introduced above, but not a problem

Earnings <- subset(Earnings, GeoFIPS>=1000) # Remove National Data
Earnings <- subset(Earnings, LineCode == 10)
Earnings <- Earnings[,c(1,2,23)]

StateEarnings <- subset(Earnings, GeoFIPS %% 1000 == 0 & GeoFIPS < 90000)
Earnings <- subset(Earnings, GeoFIPS %% 1000 != 0) # Remove non-County-level data

#Fix error in California state-level income
row.names(StateEarnings) <- NULL
California <- subset(Earnings, GeoFIPS >= 6000 & GeoFIPS <= 6115)
CalTotal <- sum(as.numeric(California$`2016`))
StateEarnings[5,3] <- CalTotal

#Add State column to county-level earnings in order to join state data
Earnings$State <- as.integer(substr(Earnings$GeoFIPS, 1, nchar(Earnings$GeoFIPS)-3)) * 1000

#Join State to County
Earnings <- Earnings %>% left_join(StateEarnings[,c(1,3)], by=c("State"="GeoFIPS"))
Earnings <- Earnings[,c(1,4,2,3,5)]
colnames(Earnings) <- c("FIPS", "State", "Name", "CountyTotal", "StateTotal")

#Calculate % of Total
Earnings$Percent <- Earnings$CountyTotal / Earnings$StateTotal



#Format GDP by State
GDPbyState <- subset(GDPbyState, IndCode == 1)
GDPbyState$`2016` <- as.numeric(GDPbyState$`2016`)

#Format Hours Worked
HoursWorked <- HoursWorked[,2:4]


#Join all data
Productivity <- Earnings %>% left_join(GDPbyState[,c(1,5)], by=c("State"="Fips"))
Productivity <- Productivity %>% left_join(HoursWorked[,c(1,3)], by=c("FIPS"="Id2"))
colnames(Productivity)[7:8] <- c("GDP", "Hours")


#Calc Productivity (THIS IS NOT WORKING)
Productivity$Productivity <- Productivity$Percent * Productivity$GDP / Productivity$Hours * 10000


#Calc Productivity
Productivity$`County Share` <- (Productivity$`County Earnings` / Productivity$`State Earnings`)
Productivity$`County GDP` <- Productivity$`County Share` * Productivity$`State GDP`
#Productivity$`Weekly Hours` <- Productivity$`Hours Worked`/Productivity$`Population 16 and Over`
#Productivity$Productivity <- Productivity$`County GDP` / Productivity$`Hours Worked` /Productivity$`Weekly Hours` * 1000 #State GDP was in millions of dollars
Productivity$Productivity <- Productivity$`County GDP` / Productivity$`Hours Worked` * 1000 #State GDP was in millions of dollars

## Growth rates for LFPR and Productivity?? (Not used yet)


### Demographic Data #################################################################

## Population structure

#By age
#Load data, from American FactFinder: ACS DEMOGRAPHIC AND HOUSING ESTIMATES, 2012-2016, 5-yr Estimates
PopStructure <- read.csv("EconData/ACS_16_5YR_DP05_with_ann.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 1)

#Format PopStructure
PopStructure <- PopStructure[,c(2, 3, 4, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 266, 290, 294, 298, 302, 306, 310, 314)]

#Delta in population structure (not done yet)


##Education
#Load population by educational attainment (% of total), data available at:
Education <- read.csv("EconData/ACS_16_5YR_S1501_with_ann.csv", sep = ",", header = T, check.names = F, stringsAsFactors = F, skip = 1)

#10-yr delta by educational attainment (not done)



## Population Density

## Crime rate
#Violent crime, data available at:
#Non-violent crime, data available at:


## Aggregated health (County Health Rankings), data available at:


### Weight data ######################################################################

#All equal

#Equal economic and demographic

#Other?

### Run analysis #####################################################################

#Group by unit set in Line 4

#Correlations

#Distances

#Create table for all units

#Filter for Top-10

#Output
##Ex: Region2: 99%, 83%, 65%, 95%, (Total: 92%)