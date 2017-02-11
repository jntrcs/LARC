#Racquetball analysis
load("RacquetballData.RData")


readFile<-function(path)
{
  dat<-read.csv(path) 
  dat$HomeTeam<-as.character(dat$HomeTeam)
  dat$AwayTeam<-as.character(dat$AwayTeam)
  dat$Date<-rep(Sys.Date()-2, nrow(dat))
  dat$Winner<-ifelse(dat$HomeScore>dat$AwayScore, dat$HomeTeam, dat$AwayTeam)
  dat$Loser<-ifelse(dat$HomeScore<dat$AwayScore, dat$HomeTeam, dat$AwayTeam)
  names(dat)<-c("Home", "HomeScore", "Visitor", "VisitorScore", "Date", "Winner", "Loser")
 dat
}
rac<-readFile("Racquetball.csv")

brierOutcomesBT<-numeric(nrow(rac))
brierOutcomesTM<-numeric(nrow(rac))
for (i in 1:nrow(rac))
{
  conf<-dataconfigure(rac[-i,])
  BTRank<-LARC.Rank(conf)
  TMRank<-LARC.Rank(conf, func=TMDensity)
  winner<-rac[i, 6]
  loser<-rac[i,7]
  winnerStrengthBT<-ifelse(length(BTRank$Strength[BTRank$Team ==winner])>0, BTRank$Strength[BTRank$Team ==winner], 1)
  loserStrengthBT<-ifelse(length(BTRank$Strength[BTRank$Team ==loser])>0, BTRank$Strength[BTRank$Team ==loser], 1)
  if (winnerStrengthBT==1|loserStrengthBT==1)
    print("Uh OH")
  winnerStrengthTM<-ifelse(length(TMRank$Strength[TMRank$Team ==winner])>0, TMRank$Strength[TMRank$Team ==winner], 0)
  loserStrengthTM<-ifelse(length(TMRank$Strength[TMRank$Team ==loser])>0, TMRank$Strength[TMRank$Team ==loser], 0)
  BTPred<-predictionPercentage(winnerStrengthBT, loserStrengthBT, "BT")
  TMPred<-predictionPercentage(winnerStrengthTM, loserStrengthTM, "TM")
  brierOutcomesBT[i]<-BTPred
  brierOutcomesTM[i]<-TMPred
}
brierScoreBT<-sum((brierOutcomesBT-1)^2)/length(brierOutcomesBT)
brierScoreTM<-sum((brierOutcomesTM-1)^2)/length(brierOutcomesTM)
brierScoreBT
brierScoreTM
