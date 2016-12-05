##Simulation function
useBT<-FALSE
Rcpp::sourceCpp("cppFiles.cpp")
simulate1<-function(useBT)
{
  simulation<-list()
  simulation$teamSchedule<-generateTeamSchedule(useBT)
  simulation$seasonGames<-generateSeasonResults(simulation$teamSchedule, useBT)
  strengths<-list()
  for (i in 1:13)
  {
    configured<-dataconfigure(simulation$seasonGames,reldate = i)
    if (i!=1)
      configured<-attachMostRecentStrengths(configured, strengths[[i-1]]$BT, strengths[[i-1]]$TM)
    strengths[[i]]<-list()
    strengths[[i]]$BT<-LARC.Rank.Football(configured, func=BTDensity, sorted=FALSE)
    strengths[[i]]$TM<-LARC.Rank.Football(configured, func=TMDensity, sorted=FALSE)
  }
  
  normTrueStrengths<-normalizeSample(simulation$teamSchedule$TrueStrength)
  #Should the mean strength be the sample or the population
  #Same with SD
  summaryOfResults<-list()
  summaryOfResults$TrueStrengthType<-ifelse(useBT, "Bradley-Terry Gamma", "Thurstone-Mosteller Normal")
  summaryOfResults$TrueStrengths<-simulation$teamSchedule$TrueStrength
  df<-data.frame(matrix(0, nrow=0, ncol=8))
  summaryOfResults$BTBias<-rep(0, 13)
  summaryOfResults$TMBias<-rep(0, 13)
  
  for (i in 1:13)
  {
    weekBT<-normalizeSample(strengths[[i]]$BT$Strength) - normTrueStrengths
    weekTM<-normalizeSample(strengths[[i]]$TM$Strength) - normTrueStrengths
    if (i!=13)
    {
      nextOpponent<-simulation$teamSchedule[,3+i]
      nextOpponent[nextOpponent==0]<-NA
      oppPredStrengthBT<-strengths[[i]]$BT$Strength[nextOpponent]
      oppPredStrengthTM<-strengths[[i]]$TM$Strength[nextOpponent]
      winPredBT<-predictionPercentage(strengths[[i]]$BT$Strength, oppPredStrengthBT, "BT")
      winPredTM<-predictionPercentage(strengths[[i]]$TM$Strength, oppPredStrengthTM, "TM")
      
    }
    else {
      
    }
    summaryOfResults$BTBias[i]<-mean(abs(weekBT))
    summaryOfResults$TMBias[i]<-mean(abs(weekTM))
    df<-rbind(df, data.frame(rep(i, 90), 1:90, strengths[[i]]$BT$Strength, strengths[[i]]$TM$Strength,
                         weekBT, weekTM, strengths[[i]]$BT$Strength - summaryOfResults$TrueStrengths,
                         strengths[[i]]$TM$Strength - summaryOfResults$TrueStrengths))
    
  }
  names(df)<-c("Week", "Team", "BTPred", "TMPred", "BTBias", "TMBias", "RawBTBias", "RawTMBias", "BTGamePredOff", "TMGamePredOff")
  summaryOfResults$Dataframe<-df
  summaryOfResults
}
#variance of estimates  + bias^2
normalizeSample<-function(strengths)
{
  meanStrength<-mean(strengths)
  sdStrength<-sd(strengths)
  norm <- (strengths- meanStrength)/sdStrength
  norm
}

plot(weekBT, normTrueStrengths)
plot(weekTM, normTrueStrengths)
summaryOfResults


mean(.4<(abs(.5-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date>4])))
mean(.4<(abs(.5-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date<=4])))

hist(abs(.5-simulation$seasonGames$HomeWinPerecent))
hist(simulation$seasonGames$HomeWinPerecent)
hist(simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date>4])
hist(simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date<=4])

system.time(season1<-simulate1(TRUE))
system.time(season2<-simulate1(FALSE))
sum(abs(season1$Dataframe$BTBias))
sum(abs(season1$Dataframe$TMBias))

plot(season1$BTBias, type='l')
lines(season1$TMBias, col='red')

plot(season2$BTBias, type='l')
lines(season2$TMBias, col='red')

v<-aggregate(season1$Dataframe$BTPred~season1$Dataframe$Week, FUN=var)
v
bt<-season1$Dataframe$BTBias

