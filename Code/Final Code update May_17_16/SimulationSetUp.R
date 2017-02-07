##Simulation function
useBT<-T
useBeta<-F
extremeBT<-F
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
  simulation$seasonGames$Outcome<-ifelse(simulation$seasonGames$Home==simulation$seasonGames$Winner, 1, 0)
  strengths<-list()
  normTrueStrengths<-simulation$teamSchedule$TrueStrength-simulation$teamSchedule$ConferenceMeans
  meanStrengths<-list()
  rejectionRate<-list()
  sdScaleBT<-seq(from=.077, to =.034,length.out=13) #chosen through trial and error to achieve close to 50% rejection rate
  sdScaleTM<-seq(from=.114, to=.054, length.out=13)
  nsamp<-1000000
  ##ESTIMATE THE SEASON   
  for (i in 1:13)
  {
    configured<-dataconfigure(simulation$seasonGames,reldate = i)
    if (i!=1)
      configured<-attachMostRecentStrengths(configured, strengths[[i-1]]$BT, strengths[[i-1]]$TM)
    strengths[[i]]<-list()
    strengths[[i]]$BT<-LARC.Rank.Football(configured, func=BTDensity, sorted=FALSE)
    strengths[[i]]$TM<-LARC.Rank.Football(configured, func=TMDensity, sorted=FALSE)
    
    meanStrengths[[i]]<-list()
    rejectionRate[[i]]<-list()
    datBT<-MetHast(logBTDensity, nSamples = nsamp, winsMatrix = configured$WinsVersus, rnormSD = sdScaleBT[i])
    rej<-datBT[[2]]
    rejectionRate[[i]]$BT<-rej/(nsamp)
    datBT<-handleBurnIn(datBT[[1]],10000)
    datBT<-useEvery(datBT,400)
    meanStrengths[[i]]$BT<-analyzeMHMatrix(datBT)
    rm(datBT)
    datTM<-MetHast(logTMDensity, nSamples = nsamp, winsMatrix = configured$WinsVersus, rnormSD = sdScaleTM[i])
    rej<-datTM[[2]]
    rejectionRate[[i]]$TM<-rej/(nsamp)
    datTM<-handleBurnIn(datTM[[1]],10000)
    datTM<-useEvery(datTM,400)
    meanStrengths[[i]]$TM<-analyzeMHMatrix(datTM)
    rm(datTM)
    
  }
  

  
  #ANALYZE THE SEASON
  summaryOfResults<-list()
  summaryOfResults$TrueStrengthType<-ifelse(useBT, ifelse(extBT, "ExtremeBT", "BradleyTerryGamma"), ifelse(beta,"Beta","ThurstoneMostellerNormal"))
  summaryOfResults$TrueStrengths<-simulation$teamSchedule$TrueStrength

  #Check Rejection rate
  summaryOfResults$RejectionRate<-list()
  summaryOfResults$RejectionRate$BT<-sapply(1:13, function(i)rejectionRate[[i]]$BT)
  summaryOfResults$RejectionRate$TM<-sapply(1:13, function(i)rejectionRate[[i]]$TM)
  
  #week over week MSE
  summaryOfResults$centeringValue<- simulation$teamSchedule$ConferenceMeans - summaryOfResults$TrueStrengths
  summaryOfResults$strengths<-list()
  summaryOfResults$strengths$BT<-list()
  summaryOfResults$strengths$BT<-lapply(1:13, FUN=function(i)strengths[[i]]$BT$Strength)
  summaryOfResults$strengths$TM<-list()
  summaryOfResults$strengths$TM<-lapply(1:13, FUN=function(i)strengths[[i]]$TM$Strength)
  summaryOfResults$strengths$BTMean<-list()
  summaryOfResults$strengths$BTMean<-lapply(1:13, FUN=function(i)meanStrengths[[i]]$BT$Mean)
  summaryOfResults$strengths$TMMean<-list()
  summaryOfResults$strengths$TMMean<-lapply(1:13, FUN=function(i)meanStrengths[[i]]$TM$Mean)
  
  ##Strength Estimate Confidence size
  summaryOfResults$CISize<-list()
  summaryOfResults$CISize$BT<-list()
  summaryOfResults$CISize$BT<-sapply(1:13, FUN=function(i)mean(meanStrengths[[i]]$BT$UpperBound-meanStrengths[[i]]$BT$LowerBound))
  summaryOfResults$CISize$TM<-list()
  summaryOfResults$CISize$TM<-sapply(1:13, FUN=function(i)mean(meanStrengths[[i]]$TM$UpperBound-meanStrengths[[i]]$TM$LowerBound))
  
  #Correlation
  summaryOfResults$SpearmanCorrelation<-list()
  summaryOfResults$SpearmanCorrelation$BT<-rep(0,13)
  summaryOfResults$SpearmanCorrelation$TM<-rep(0,13)
  summaryOfResults$SpearmanCorrelation$BTMean<-rep(0,13)
  summaryOfResults$SpearmanCorrelation$TMMean<-rep(0,13)
  for (i in 1:13)
  {
    summaryOfResults$SpearmanCorrelation$BT[i]<-cor(summaryOfResults$TrueStrengths, strengths[[i]]$BT$Strength, method="spearman")
    summaryOfResults$SpearmanCorrelation$TM[i]<-cor(summaryOfResults$TrueStrengths, strengths[[i]]$TM$Strength, method="spearman")
    summaryOfResults$SpearmanCorrelation$BTMean[i]<-cor(summaryOfResults$TrueStrengths, meanStrengths[[i]]$BT$Mean, method="spearman")
    summaryOfResults$SpearmanCorrelation$TMMean[i]<-cor(summaryOfResults$TrueStrengths, meanStrengths[[i]]$TM$Mean, method="spearman")
  }

  #Game Bias MSE
  BTGamePred<-rep(0, 540)
  TMGamePred<-rep(0,540)
  BTMeanGamePred<-rep(0, 540)
  TMMeanGamePred<-rep(0,540)
  week<-numeric()
  for (i in 1:nrow(simulation$seasonGames))
  {
    cweek<-simulation$seasonGames$Date[i]
    if (cweek==1)
    {
      BTGamePred[i]<-NA
      BTGamePred[i]<-NA
      BTMeanGamePred[i]<-NA
      TMMeanGamePred[i]<-NA
    }
    else
    {
      homeStrengthBT<-strengths[[cweek-1]]$BT$Strength[simulation$seasonGames$Home[i]]
      homeStrengthBTMean<-meanStrengths[[cweek-1]]$BT$Mean[simulation$seasonGames$Home[i]]
      
      homeStrengthTM<-strengths[[cweek-1]]$TM$Strength[simulation$seasonGames$Home[i]]
      homeStrengthTMMean<-meanStrengths[[cweek-1]]$TM$Mean[simulation$seasonGames$Home[i]]
      
      awayStrengthBT<-strengths[[cweek-1]]$BT$Strength[simulation$seasonGames$Visitor[i]]
      awayStrengthBTMean<-meanStrengths[[cweek-1]]$BT$Mean[simulation$seasonGames$Visitor[i]]
      
      awayStrengthTM<-strengths[[cweek-1]]$TM$Strength[simulation$seasonGames$Visitor[i]]
      awayStrengthTMMean<-meanStrengths[[cweek-1]]$TM$Mean[simulation$seasonGames$Visitor[i]]
      
      BTGamePred[i]<-predictionPercentage(homeStrengthBT, awayStrengthBT, "BT")
      BTMeanGamePred[i]<-predictionPercentage(homeStrengthBTMean, awayStrengthBTMean, "BT")
      
      TMGamePred[i]<-predictionPercentage(homeStrengthTM, awayStrengthTM, "TM")
      TMMeanGamePred[i]<-predictionPercentage(homeStrengthTMMean, awayStrengthTMMean, "TM")
      
      week[i]<-cweek
      
      
    }
    
    
  }
  favoredRealPred<-simulation$seasonGames$HomeWinPerecent
  summaryOfResults$GameBias<-data.frame(week[46:540],(BTGamePred)[46:540], (BTMeanGamePred)[46:540],
                                        
                                        (TMGamePred)[46:540],(TMMeanGamePred)[46:540],
                                        favoredRealPred[46:540], simulation$seasonGames$Outcome[46:540])
  names(summaryOfResults$GameBias)<-c("Week","BTGamePrediction", "BTMeanGamePrediction", 
                                      "TMGamePrediction", "TMMeanGamePrediction", "ActualGame", "ActualOutcome")
  summaryOfResults$GameBiasByWeek<-analyzeGameBias(summaryOfResults$GameBias)
  summaryOfResults$disparityScore<-sum(abs(summaryOfResults$GameBias$ActualGame-.5))/nrow(summaryOfResults$GameBias)
  summaryOfResults
}

analyzeGameBias<-function(gamebias)
{
  mseBT<-findMSE(gamebias$Week, gamebias$BTGamePrediction, gamebias$ActualGame)
  mseTM<-findMSE(gamebias$Week, gamebias$TMGamePrediction, gamebias$ActualGame)
  mseBTMean<-findMSE(gamebias$Week, gamebias$BTMeanGamePrediction, gamebias$ActualGame)
  mseTMMean<-findMSE(gamebias$Week, gamebias$TMMeanGamePrediction, gamebias$ActualGame)
  a<-merge(mseBT, mseTM, by=c("weeks"))
  a<-merge(a, mseBTMean, by=c("weeks"))
  names(a)<-c("weeks", "a", "b", "c")
  a<-merge(a, mseTMMean, by=c("weeks"))
  
  names(a)<-c("Week", "BT.MSE", "TM.MSE", "BTMean.MSE", "TMMean.MSE")
  a
}




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

####OLD CODE
normTrueStrengths<-normalizeSample(simulation$teamSchedule$TrueStrength)
#Should the mean strength be the sample or the population
#Same with SD

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