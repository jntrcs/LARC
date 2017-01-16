#Win probability study
TMStrengths$Strength[which(TMStrengths$Team==weekGames$Home[i])]

library(qdapTools)
winnersStrength<-lookup(strippedRaw$Winner, as.character(stripped2016data[[15]][[2]]$Team), key.reassign=stripped2016data[[15]][[2]]$Strength)
losersStrength<-lookup(strippedRaw$Loser, as.character(stripped2016data[[15]][[2]]$Team), key.reassign=stripped2016data[[15]][[2]]$Strength)

winnersWinProb<-(winnersStrength)/(winnersStrength+losersStrength)

winnersStrengthTM<-lookup(strippedRaw$Winner, as.character(stripped2016data[[15]][[3]]$Team), key.reassign=stripped2016data[[15]][[3]]$Strength)
losersStrengthTM<-lookup(strippedRaw$Loser, as.character(stripped2016data[[15]][[3]]$Team), key.reassign=stripped2016data[[15]][[3]]$Strength)

winnersWinProbTM<-pnorm(winnersStrengthTM-losersStrengthTM)

boxplot(winnersWinProb, winnersWinProbTM)

par(mfrow=c(2,1))
hist(winnersWinProb, main="Bradley-Terry", xlim=c(0,1))
hist(winnersWinProbTM, main="TM", xlim=c(0,1))
