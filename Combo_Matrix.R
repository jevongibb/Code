library(dplyr)
library(tidyr)
library(reshape2)

#Load matrices
LC_Emp <- read.csv("Matrices/AvgDist_Emp_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
LC_Est <- read.csv("Matrices/AvgDist_Est_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
Occ <- read.csv("Matrices/AvgDist_Occ_2011to2015.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)
IO <- read.csv("Matrices/IO_data_dist.csv", header = T, sep = ",", stringsAsFactors = F, check.names = F, row.names = 1)

#Test correlation
cor(c(as.matrix(LC_Emp)), c(as.matrix(LC_Est)))
cor(c(as.matrix(LC_Emp)), c(as.matrix(IO)))
cor(c(as.matrix(LC_Est)), c(as.matrix(IO)))

#Shrink Matrices to match Occ
Remove <- colnames(LC_Emp)
Remove2 <- colnames(Occ)
Remove <- Remove[!(Remove %in% Remove2)]
rm(Remove2)
LC_Emp2 <- LC_Emp[!rownames(LC_Emp) %in% Remove, !colnames(LC_Emp) %in% Remove]
LC_Est2 <- LC_Est[!rownames(LC_Est) %in% Remove, !colnames(LC_Est) %in% Remove]
IO2 <- IO[!rownames(IO) %in% Remove, !colnames(IO) %in% Remove]

#Occ correlations
cor(c(as.matrix(LC_Emp2)), c(as.matrix(Occ)))
cor(c(as.matrix(LC_Est2)), c(as.matrix(Occ)))
cor(c(as.matrix(IO2)), c(as.matrix(Occ)))


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

Occ3 <- Occ
Occ3$NAICS <- colnames(Occ3)
Occ3 <- Occ3[,c(658, 1:657)]
Occ3 <- melt(Occ3)
colnames(Occ3) <- c("Industry1", "Industry2", "Distance")

Combo <- LC_Emp3
Combo <- Combo %>% left_join(LC_Est3, by=c("Industry1", "Industry2"))
Combo <- Combo %>% left_join(IO3, by=c("Industry1", "Industry2"))
Occ3$Industry2 <- as.character(Occ3$Industry2)
Combo <- Combo %>% left_join(Occ3, by=c("Industry1", "Industry2"))

colnames(Combo)[3:6] <- c("LC_Emp", "LC_Est", "IO", "Occ")


### Standardize Distances##############################################

#Remove same-same pairs
Combo <- Combo[Combo$Industry1 != Combo$Industry2,]

#standardize
Combo$Scale_LC_Emp <- scale(Combo$LC_Emp)
Combo$Scale_LC_Est <- scale(Combo$LC_Est)
Combo$Scale_Occ <- scale(Combo$Occ)
Combo$Scale_IO <- scale(Combo$IO)

Combo$Scale <- rowMeans(Combo[,7:10], na.rm = T)


### Export standardized distances as a matrix #########################

NewCombo <- spread(Combo[,c(1,2,11)], Industry2, Scale, fill = NA)
rownames(NewCombo) <- NewCombo$Industry1
NewCombo$Industry1 <- NULL
NewCombo <- as.matrix(NewCombo)

write.csv(NewCombo, "Matrices/NewCombo.csv")
