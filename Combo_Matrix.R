library(dplyr)
library(reshape2)

#Load matrices
LC_Emp <- read.csv("Matrices/AvgDist_Emp_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
LC_Est <- read.csv("Matrices/AvgDist_Est.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
OCC <- read.csv("Matrices/AvgDist_Occ.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
IO <- read.csv("Matrices/IO_data.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#Test correlation
CorEmpEnt <- cor(c(as.matrix(LC_Emp)), c(as.matrix(LC_Est)))
CorEmpIO <- cor(c(as.matrix(LC_Emp)), c(as.matrix(IO)))
CoeEstIO <- cor(c(as.matrix(LC_Est)), c(as.matrix(IO)))

#Shrink Matrices to match OCC
Remove <- colnames(LC_Emp)
Remove2 <- colnames(OCC)
Remove <- Remove[!(Remove %in% Remove2)]
rm(Remove2)
LC_Emp2 <- LC_Emp[!rownames(LC_Emp) %in% Remove, !colnames(LC_Emp) %in% Remove]
LC_Est2 <- LC_Est[!rownames(LC_Est) %in% Remove, !colnames(LC_Est) %in% Remove]
IO2 <- IO[!rownames(IO) %in% Remove, !colnames(IO) %in% Remove]

#OCC correlations
CorEmpOCC <- cor(c(as.matrix(LC_Emp2)), c(as.matrix(OCC)))
CorEstOCC <- cor(c(as.matrix(LC_Est2)), c(as.matrix(OCC)))
CorIOOCC <- cor(c(as.matrix(IO2)), c(as.matrix(OCC)))


#Combine matrices
LC_Emp3 <- LC_Emp
LC_Emp3$NAICS <- colnames(LC_Emp3)
LC_Emp3 <- LC_Emp3[,c(676, 1:675)]
LC_Emp3 <- melt(LC_Emp3)
colnames(LC_Emp3) <- c("Industry1", "Industry2", "Distance")

LC_Est3 <- LC_Est
LC_Est3$NAICS <- colnames(LC_Est3)
LC_Est3 <- LC_Est3[,c(676, 1:675)]
LC_Est3 <- melt(LC_Est3)
colnames(LC_Est3) <- c("Industry1", "Industry2", "Distance")

IO3 <- IO
IO3$NAICS <- colnames(IO3)
IO3 <- IO3[,c(676, 1:675)]
IO3 <- melt(IO3)
colnames(IO3) <- c("Industry1", "Industry2", "Distance")

OCC3 <- OCC
OCC3$NAICS <- colnames(OCC3)
OCC3 <- OCC3[,c(658, 1:657)]
OCC3 <- melt(OCC3)
colnames(OCC3) <- c("Industry1", "Industry2", "Distance")

Combo <- LC_Emp3
Combo <- Combo %>% left_join(LC_Est3, by=c("Industry1", "Industry2"))
Combo <- Combo %>% left_join(IO3, by=c("Industry1", "Industry2"))
OCC3$Industry2 <- as.character(OCC3$Industry2)
Combo <- Combo %>% left_join(OCC3, by=c("Industry1", "Industry2"))

colnames(Combo)[3:6] <- c("LC_Emp", "LC_Est", "IO", "OCC")


### Standardize Distances##############################################

#Remove same-same pairs
Combo <- Combo[Combo$Industry1 != Combo$Industry2,]

#generate percentrank
Combo$Test <- rank(Combo$LC_Emp, ties.method = "min", na.last = NA) / sum(!is.na(Combo$LC_Emp))
Combo$Scale <- scale(Combo$LC_Emp)

#normalize
normalize <- function(x){(x-min(x))/(max(x)-min(x))}
Combo$Normalize <- normalize(Combo$LC_Emp)


#I think Test is better than Scale