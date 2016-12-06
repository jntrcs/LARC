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
                         strengths[[i]]$TM$Strength - summaryOfResults$TrueStrengths, winPredOffBT, winPredOffTM))
    
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
  
  summaryOfResults$GameBias<-data.frame(week[46:540],(BTGamePred-favoredRealPred)[46:540], 
                                        (TMGamePred-favoredRealPred)[46:540])
  names(summaryOfResults$GameBias)<-c("Week","BTGamePredictionBias", "TMGamePredictionBias")
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
season1$GameBias

a<-summaryOfResults$GameBias
d<-aggregate(a$BTGamePredictionBias~a$Week, FUN=mean)
plot(d$`a$BTGamePredictionBias`)
e<-aggregate(a$TMGamePredictionBias~a$Week, FUN=mean)
plot(e$`a$TMGamePredictionBias`)
e
hist(a$BTGamePredictionBias)
hist(a$BTGamePredictionBias[a$Week==13])
hist(a$TMGamePredictionBias)
hist(a$TMGamePredictionBias[a$Week==13])
