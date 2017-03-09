#Investigation into the win probability of each game using end of season strengths on real data

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

###2015
winnersStrength<-lookup(strippedRaw$Winner, as.character(stripped2015data[[14]][[2]]$Team), key.reassign=stripped2015data[[14]][[2]]$Strength)
losersStrength<-lookup(strippedRaw$Loser, as.character(stripped2015data[[14]][[2]]$Team), key.reassign=stripped2015data[[14]][[2]]$Strength)

winnersWinProb<-(winnersStrength)/(winnersStrength+losersStrength)

winnersStrengthTM<-lookup(strippedRaw$Winner, as.character(stripped2015data[[14]][[3]]$Team), key.reassign=stripped2015data[[14]][[3]]$Strength)
losersStrengthTM<-lookup(strippedRaw$Loser, as.character(stripped2015data[[14]][[3]]$Team), key.reassign=stripped2015data[[14]][[3]]$Strength)

winnersWinProbTM<-pnorm(winnersStrengthTM-losersStrengthTM)

boxplot(winnersWinProb, winnersWinProbTM)

par(mfrow=c(2,1))
hist(winnersWinProb, main="Bradley-Terry 2015", xlim=c(0,1))
hist(winnersWinProbTM, main="TM 2015", xlim=c(0,1))

sum(1-winnersWinProbTM)
