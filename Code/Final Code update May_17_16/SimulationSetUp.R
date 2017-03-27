##The main function to simulate an entire football season

useBT<-TRUE
useBeta<-FALSE
extremeBT<-TRUE
load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

simulate1<-function(useBT, useBeta = FALSE, extremeBT=FALSE)
{
  ##SIMULATE A SEASON
  beta <-!useBT & useBeta & !extremeBT
  extBT<-useBT & extremeBT
  simulation<-list()
  simulation$teamSchedule<-generateTeamSchedule(useBT, beta, extBT)
  simulation$seasonGames<-generateSeasonResults(simulation$teamSchedule, useBT, beta)
  
  strengths<-list()
  normTrueStrengths<-simulation$teamSchedule$TrueStrength-simulation$teamSchedule$ConferenceMeans
##eliminate everything after week 4
    simulation$seasonGames<-simulation$seasonGames[simulation$seasonGames$Date<=4,]
    simulation$teamSchedule<-simulation$teamSchedule[,1:8]
  ##ESTIMATE THE SEASON   
  for (i in 1:4)
  {
    configured<-dataconfigure(simulation$seasonGames,reldate = i)
    if (i!=1)
      configured<-attachMostRecentStrengths(configured, strengths[[i-1]]$BT, strengths[[i-1]]$TM)
    strengths[[i]]<-list()
    strengths[[i]]$BT<-LARC.Rank.Football(configured, func=BTDensity, sorted=FALSE)
    strengths[[i]]$TM<-LARC.Rank.Football(configured, func=TMDensity, sorted=FALSE)
  }
  for (i in 5:12)
  {
    if (i%%2==0) Trank <- order(-strengths[[i-1]]$TM$Strength) else 
      Trank<- order(-strengths[[i-1]]$BT$Strength)
      simulation$teamSchedule<-cbind(simulation$teamSchedule, getWeekMatchups(Trank))
      names(simulation$teamSchedule)[i+4]<-paste0("Week",i)
      simulation$seasonGames<-rbind(simulation$seasonGames, generateWeekResults(simulation$teamSchedule, i, useBT, beta))
      configured<-dataconfigure(simulation$seasonGames,reldate = i)
      if (i!=1)
        configured<-attachMostRecentStrengths(configured, strengths[[i-1]]$BT, strengths[[i-1]]$TM)
      strengths[[i]]<-list()
      strengths[[i]]$BT<-LARC.Rank.Football(configured, func=BTDensity, sorted=FALSE)
      strengths[[i]]$TM<-LARC.Rank.Football(configured, func=TMDensity, sorted=FALSE)
  }
  
  #ANALYZE THE SEASON
  summaryOfResults<-list()
  summaryOfResults$TrueStrengthType<-ifelse(useBT, ifelse(extBT, "ExtremeBT", "BradleyTerryGamma"), ifelse(beta,"Beta","ThurstoneMostellerNormal"))
  summaryOfResults$TrueStrengths<-simulation$teamSchedule$TrueStrength

  #week over week MSE
  summaryOfResults$centeringValue<- simulation$teamSchedule$ConferenceMeans - summaryOfResults$TrueStrengths
  summaryOfResults$strengths<-list()
  summaryOfResults$strengths$BT<-list()
  summaryOfResults$strengths$BT<-lapply(1:12, FUN=function(i)strengths[[i]]$BT$Strength)
  summaryOfResults$strengths$TM<-list()
  summaryOfResults$strengths$TM<-lapply(1:12, FUN=function(i)strengths[[i]]$TM$Strength)
  
  #Correlation
  summaryOfResults$SpearmanCorrelation<-list()
  summaryOfResults$SpearmanCorrelation$BT<-rep(0,12)
  summaryOfResults$SpearmanCorrelation$TM<-rep(0,12)
  for (i in 1:12)
  {
    summaryOfResults$SpearmanCorrelation$BT[i]<-cor(summaryOfResults$TrueStrengths, strengths[[i]]$BT$Strength, method="spearman")
    summaryOfResults$SpearmanCorrelation$TM[i]<-cor(summaryOfResults$TrueStrengths, strengths[[i]]$TM$Strength, method="spearman")
  }
  #plot(summaryOfResults$SpearmanCorrelation$BT, main="BT True", xlab="week", ylab="Spearmans Correlation", ylim=c(.2,.9))
  #points(summaryOfResults$SpearmanCorrelation$TM, col="red")
  #plot(strengths[[13]]$BT$Strength, summaryOfResults$TrueStrengths)
  
  #Game Bias MSE
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
  #BTGamePred[simulation$seasonGames$HomeWinPerecent<.5]<-1-BTGamePred[simulation$seasonGames$HomeWinPerecent<.5]
  favoredRealPred<-simulation$seasonGames$HomeWinPerecent
  #favoredRealPred[simulation$seasonGames$HomeWinPerecent<.5]<-1-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$HomeWinPerecent<.5]
  #TMGamePred[simulation$seasonGames$HomeWinPerecent<.5]<-1-TMGamePred[simulation$seasonGames$HomeWinPerecent<.5]
  
  summaryOfResults$GameBias<-data.frame(week[46:540],(BTGamePred)[46:540], 
                                        (TMGamePred)[46:540], favoredRealPred[46:540])
  names(summaryOfResults$GameBias)<-c("Week","BTGamePrediction", "TMGamePrediction", "ActualGame")
  summaryOfResults$GameBiasByWeek<-analyzeGameBias(summaryOfResults$GameBias)
  summaryOfResults$disparityScore<-sum(abs(summaryOfResults$GameBias$ActualGame-.5))/nrow(summaryOfResults$GameBias)
  summaryOfResults
}

analyzeGameBias<-function(gamebias)
{
  mseBT<-findMSE(gamebias$Week, gamebias$BTGamePrediction, gamebias$ActualGame)
  mseTM<-findMSE(gamebias$Week, gamebias$TMGamePrediction, gamebias$ActualGame)
  a<-merge(mseBT, mseTM, by=c("weeks"))
  names(a)<-c("Week", "BT.MSE", "TM.MSE")
  a
}
#variance of estimates  + bias^2
#normalizeSample<-function(strengths)
#{
#  meanStrength<-mean(strengths)
#  sdStrength<-sd(strengths)
#  norm <- (strengths- meanStrength)/sdStrength
#  norm
#}



findMSE<-function(weeks, gamePred, actual)
{
  MSE <- gamePred-actual
  aggregate(MSE~weeks, FUN=function(i){sum(i^2)/length(i)})
}

plot(weekBT, normTrueStrengths)
plot(weekTM, normTrueStrengths)

mean(.4<(abs(.5-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date>4])))
mean(.4<(abs(.5-simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date<=4])))

hist(abs(.5-simulation$seasonGames$HomeWinPerecent))
hist(simulation$seasonGames$HomeWinPerecent)
hist(simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date>4])
hist(simulation$seasonGames$HomeWinPerecent[simulation$seasonGames$Date<=4])

system.time(season1<-simulate1(FALSE, TRUE))
system.time(season2<-simulate1(FALSE))



mseBT<-findMSE(season1$GameBias$Week, season1$GameBias$BTGamePrediction, season1$GameBias$ActualGame)
mseTM<-findMSE(season1$GameBias$Week, season1$GameBias$TMGamePrediction, season1$GameBias$ActualGame)
plot(mseBT$MSE~mseBT$weeks, main="With Beta Strengths", xlab="Week", ylab="MSE", ylim=c(0,.1))
points(mseTM$MSE~mseTM$weeks, col="Red")
legend("topright", c("BT", "TM"), lty=c(1,1), col=c("Black", "red"))

##with the TM strengths
mseBT2<-findMSE(season2$GameBias$Week, season2$GameBias$BTGamePrediction, season2$GameBias$ActualGame)
mseTM2<-findMSE(season2$GameBias$Week, season2$GameBias$TMGamePrediction, season2$GameBias$ActualGame)
plot(mseBT2$MSE~mseTM2$weeks, main="With TM Strengths", ylim=c(0,.13), type='l')
lines(mseTM2$MSE~mseTM2$weeks, col="Red")
legend("topright", c("BT", "TM"), lty=c(1,1), col=c("Black", "red"))

btVar<-aggregate(BTGamePrediction~Week, FUN=var, data=season1$GameBias)
difBT<-season1$GameBias$BTGamePrediction-season1$GameBias$ActualGame
mseBT$Bias<-sqrt(mseBT$difBT-btVar$BTGamePrediction) ##Not sure how I'd end up with an imaginary bias



plot(mseBT$`season1$GameBias$Week`, mseBT$`season1$GameBias$BTGamePredictionBias`, ylim=c(0,.12))
points(mseTM$`season1$GameBias$Week`, mseTM$`season1$GameBias$TMGamePredictionBias`,col="Red")

plot(mseBT2$`season2$GameBias$Week`, mseBT2$`season2$GameBias$BTGamePredictionBias`, ylim=c(0,.12))
points(mseTM2$`season2$GameBias$Week`, mseTM2$`season2$GameBias$TMGamePredictionBias`,col="Red")

week<-13
a<-season1$GameBias$BTGamePrediction[season1$GameBias$Week==week] - season1$GameBias$ActualGame[season1$GameBias$Week==week]
b<-season1$GameBias$TMGamePrediction[season1$GameBias$Week==week] - season1$GameBias$ActualGame[season1$GameBias$Week==week]
hist(a)
hist(b)
mean(a)
mean(b)

