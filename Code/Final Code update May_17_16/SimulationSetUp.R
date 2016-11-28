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
    strengths[[i]]$BT<-LARC.Rank.Football(configured, func=BTDensity, sort=FALSE)
    strengths[[i]]$TM<-LARC.Rank.Football(configured, func=TMDensity, sort=FALSE)
  }
  
  normTrueStrengths<-normalizeSample(simulation$teamSchedule$TrueStrength)
  #Should the mean strength be the sample or the population
  #Same with SD
  summaryOfResults<-list()
  summaryOfResults$TrueStrengthType<-ifelse(useBT, "Bradley-Terry Gamma", "Thurstone-Mosteller Normal")
  summaryOfResults$TrueStrengths<-simulation$teamSchedule$TrueStrength
  nrow<-1170
  df<-data.frame(matrix(0, nrow=0, ncol=8))
  names(df)<-c("Week", "Team", "BTPred", "TMPred", "BTBias", "TMBias", "RawBTBias", "RawTMBias")
  for (i in 1:13)
  {
    summaryOfResults[[i]]<-list()
    weekBT<-normalizeSample(strengths[[i]]$BT$Strength)
    weekTM<-normalizeSample(strengths[[i]]$TM$Strength)
    summaryOfResults[[i]]$BTBias<-mean(abs(normTrueStrengths-weekBT))
    summaryOfResults[[i]]$TMBias<-mean(sqrt((normTrueStrengths-weekTM)^2))
    
  }
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
