options(scipen=999)
library(dplyr)

#Choose whether to group by County, MSA, or Commuting Zone
#Load region definitions

#Choose region to run cohort analysis

### Economic Data ####################################################################

## Industry composition
#Load industry composition by employees for all counties in most recent year (result of custom algorithm)
#Raw data available at:
#Algorithm available at:

##Income
#10-yr Income by quintile, data available at:

#10-yr Gini

##Labor data
#10-yr Labor force participation rate (LFPR), data available at:
#Filter for Prime-Age LFPR, data available at:

#10-yr Unemployment rate, data available at:

#% of people living in poverty, data available at:

##Output
#10-yr GDP, data available at:
#Growth rate

#10-yr GDP per capita, data available at:
#Growth rate

### Demographic Data #################################################################

## Population
#Load 10-yr population, data available at:

#Convert to delta in population

## Age structure
#Load number of people by age group for all counties (most recent year)
#data available at:

#10-yr delta by Age

## Race
#Load number of people by race for all counties (most recent year)
#data available at:

#10-yr delta by race

##Education
#Load population by educational attainment (% of total), data available at:

#10-yr delta by educational attainment (by % of total)

## Population Density

## Crime rate
#Violent crime, data available at:
#Non-violent crime, data available at:

#% of households living in owner-occupied home

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