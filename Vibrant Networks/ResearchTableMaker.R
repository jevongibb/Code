NewCombo[is.na(NewCombo)] <- 0

Current <- NewCombo

mean(as.matrix(Current))
sd(as.matrix(Current))

min_mat <- as.matrix(Current)
min_mat[min_mat==0] <- NA
min(min_mat, na.rm = T)

max(as.matrix(Current))
median(as.matrix(Current))
quantile(as.matrix(Current),.9)


#Cor matrix



Remove <- colnames(NewCombo)
Remove2 <- colnames(Occ)
Remove <- Remove[!(Remove %in% Remove2)]
rm(Remove2)
NewCombo2 <- NewCombo[!rownames(NewCombo) %in% Remove, !colnames(NewCombo) %in% Remove]

cor(c(as.matrix(NewCombo)), c(as.matrix(LC_Emp)))
cor(c(as.matrix(NewCombo)), c(as.matrix(LC_Est)))
cor(c(as.matrix(NewCombo)), c(as.matrix(IO)))
cor(c(as.matrix(NewCombo2)), c(as.matrix(Occ)))
