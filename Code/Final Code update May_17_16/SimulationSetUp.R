##Simulation function
useBT<-TRUE
load("MasterFunctionFile.RData")
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

    summaryOfResults$BTBias[i]<-mean(abs(weekBT))
    summaryOfResults$TMBias[i]<-mean(abs(weekTM))
    df<-rbind(df, data.frame(rep(i, 90), 1:90, strengths[[i]]$BT$Strength, strengths[[i]]$TM$Strength,
                         weekBT, weekTM, strengths[[i]]$BT$Strength - summaryOfResults$TrueStrengths,
                         strengths[[i]]$TM$Strength - summaryOfResults$TrueStrengths))
    
  }
  names(df)<-c("Week", "Team", "BTPred", "TMPred", "BTBias", "TMBias", "RawBTBias", "RawTMBias")
  summaryOfResults$Dataframe<-df
  BTGamePred<-rep(0, 540)
  TMGamePred<-rep(0,540)
  week<-numeric()
  for (i in 1:nrow(simulation$seasonGames))
  {
    cweek<-simulation$seasonGames$Date[i]
    if (cweek==1)
    {
      BTGamePred[i]<-NA
      BTGamePred[i]<-NA
    }
    else
    {
      homeStrengthBT<-strengths[[cweek-1]]$BT$Strength[simulation$seasonGames$Home[i]]
      homeStrengthTM<-strengths[[cweek-1]]$TM$Strength[simulation$seasonGames$Home[i]]
      awayStrengthBT<-strengths[[cweek-1]]$BT$Strength[simulation$seasonGames$Visitor[i]]
      awayStrengthTM<-strengths[[cweek-1]]$TM$Strength[simulation$seasonGames$Visitor[i]]
      BTGamePred[i]<-predictionPercentage(homeStrengthBT, awayStrengthBT, "BT")
      TMGamePred[i]<-predictionPercentage(homeStrengthTM, awayStrengthTM, "TM")
      week[i]<-cweek
      
      
    }
    
  }
  BTGamePred[simulation$seasonGames$HomeWinPerecent<.5]<-1-BTGamePred[simulation$seasonGames$HomeWinPerecent<.5]
  favoredRealPred<-simulation$seasonGames$HomeWinPerecent
  favoredRealPred[simulation$seasonGames$HomeWinPerecent<.5]<-1-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$HomeWinPerecent<.5]
  TMGamePred[simulation$seasonGames$HomeWinPerecent<.5]<-1-TMGamePred[simulation$seasonGames$HomeWinPerecent<.5]
  
  summaryOfResults$GameBias<-data.frame(week[46:540],(BTGamePred)[46:540], 
                                        (TMGamePred)[46:540], favoredRealPred[46:540])
  names(summaryOfResults$GameBias)<-c("Week","BTGamePrediction", "TMGamePrediction", "ActualGame")
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

calcMSE<-function(pred, actual)
{
  dif<-pred-actual
  sum(i^2)/length(i)
}

plot(weekBT, normTrueStrengths)
plot(weekTM, normTrueStrengths)

mean(.4<(abs(.5-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date>4])))
mean(.4<(abs(.5-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date<=4])))

hist(abs(.5-simulation$seasonGames$HomeWinPerecent))
hist(simulation$seasonGames$HomeWinPerecent)
hist(simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date>4])
hist(simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date<=4])

system.time(season1<-simulate1(TRUE))
system.time(season2<-simulate1(FALSE))

findMSE<-function(weeks, gamePred, actual)
{
  MSE <- gamePred-actual
  aggregate(MSE~weeks, FUN=function(i){sum(i^2)/length(i)})
}

mseBT<-findMSE(season1$GameBias$Week, season1$GameBias$BTGamePrediction, season1$GameBias$ActualGame)
mseTM<-findMSE(season1$GameBias$Week, season1$GameBias$TMGamePrediction, season1$GameBias$ActualGame)
plot(mseBT$MSE~mseBT$weeks, main="With Bradley Terry Strengths")
points(mseTM$MSE~mseTM$weeks, col="Red")
legend("topright", c("BT", "TM"), lty=c(1,1), col=c("Black", "red"))

##with the BT strengths
mseBT2<-findMSE(season2$GameBias$Week, season2$GameBias$BTGamePrediction, season2$GameBias$ActualGame)
mseTM2<-findMSE(season2$GameBias$Week, season2$GameBias$TMGamePrediction, season2$GameBias$ActualGame)
plot(mseBT2$MSE~mseBT2$weeks, main="With TM Strengths")
points(mseTM2$MSE~mseTM2$weeks, col="Red")
legend("topright", c("BT", "TM"), lty=c(1,1), col=c("Black", "red"))

btVar<-aggregate(BTGamePrediction~Week, FUN=var, data=season1$GameBias)
difBT<-season1$GameBias$BTGamePrediction-season1$GameBias$ActualGame
mseBT$Bias<-sqrt(mseBT$difBT-btVar$BTGamePrediction) ##Not sure how I'd end up with an imaginary bias



plot(mseBT$`season1$GameBias$Week`, mseBT$`season1$GameBias$BTGamePredictionBias`, ylim=c(0,.12))
points(mseTM$`season1$GameBias$Week`, mseTM$`season1$GameBias$TMGamePredictionBias`,col="Red")

plot(mseBT2$`season2$GameBias$Week`, mseBT2$`season2$GameBias$BTGamePredictionBias`, ylim=c(0,.12))
points(mseTM2$`season2$GameBias$Week`, mseTM2$`season2$GameBias$TMGamePredictionBias`,col="Red")
