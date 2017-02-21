#### NCAAF Predictor
#The purpose of this script is to use the strengths after a given week of football (week 2) to predict the
#next weeks games (week 3). And then to assess which function did so with a higher accuracy.

#TMStrengths and BTStrengths are the Thurston-Mosteller and Bradley-Terry strengths produced by LARC.Rank
#Schedule is the df produced by datascrape
#Datevector is length two, beginning with the day to start and the day to end predictions for.

#The return value is a list. The first item is a dataframe containing all games predicted and info on predictions
#The second value is a table of which model more confidently predicted the winner
#The third value is the penalties assessed. Remember the penalty is computed thus: if the model's prediction
#was further from the actual result (1 for win, 0 for loss), then that team is assessed a penalty of the 
#distance from their prediction to the correct answer. This gives us the desirable property that if Team A
#beat Team B 95 out of 100 times, and model A predicted team A at 95% and Model B at 99%, 
#Model B would be penalized more for it's 5 misses then A for it's 95 misses.
#The fourth item is the average difference (absolute value) between the two prediction methods
#The fifth item is number of games predicted


NCAAFPredictor<-function(BTStrengths,TMStrengths, schedule, dateVector)
{
  TMStrengths$Team<-as.character(TMStrengths$Team)
  BTStrengths$Team<-as.character(BTStrengths$Team)
  startdate<-as.Date(dateVector[1])
  enddate<-as.Date(dateVector[2])
  
  
  weekGames<-schedule[schedule$Date<enddate&schedule$Date>startdate, c(3,5,8,9)]  
  
  
  for (i in 1:nrow(weekGames))
  {
    
    #This finds the strengths of the teams.
    #The messy if elses are to handle the case where they play a team that has no previously estimated strength
    weekGames$HomeTMStength[i]<-ifelse(weekGames$Home[i]%in%TMStrengths$Team,TMStrengths$Strength[which(TMStrengths$Team==weekGames$Home[i])],0)
    weekGames$AwayTMStrength[i]<-ifelse(weekGames$Visitor[i]%in%TMStrengths$Team,TMStrengths$Strength[which(TMStrengths$Team==weekGames$Visitor[i])],0)
    weekGames$AwayBTStrength[i]<-ifelse(weekGames$Visitor[i]%in%BTStrengths$Team,BTStrengths$Strength[which(BTStrengths$Team==weekGames$Visitor[i])],1)
    weekGames$HomeBTStrength[i]<-ifelse(weekGames$Home[i]%in%BTStrengths$Team,BTStrengths$Strength[which(BTStrengths$Team==weekGames$Home[i])],1)
  }
  
  weekGames$BTHomeWin<-weekGames$HomeBTStrength/(weekGames$HomeBTStrength+weekGames$AwayBTStrength)
  weekGames$TMHomeWin<-pnorm(weekGames$HomeTMStength-weekGames$AwayTMStrength)
  weekGames$Difference<-abs(weekGames$BTHomeWin-weekGames$TMHomeWin)
  weekGames$HomeTeamWon<-weekGames$Home==weekGames$Winner
  #This line looks at which prediction was closer to the actual value of who won and who lost
  weekGames$DidBetter<-ifelse(abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$BTHomeWin)<abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$TMHomeWin), "Bradley-Terry", "Thurstone-Mosteller")
  weekGames$DidBetter[weekGames$BTHomeWin == weekGames$TMHomeWin]<-"Tie"
  weekGames$DidWorse<-ifelse(weekGames$DidBetter=="Bradley-Terry", "Thurstone-Mosteller", "Bradley-Terry")
  weekGames$DidWorse[weekGames$DidBetter=="Tie"]<-"Tie"
  weekGames$Penalty <- ifelse(weekGames$DidWorse =="Bradley-Terry", abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$BTHomeWin), abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$TMHomeWin))
  weekGames$BrierComponentBT<-weekGames$BTHomeWin - ifelse(weekGames$HomeTeamWon, 1, 0)
  weekGames$BrierComponentTM<-weekGames$TMHomeWin - ifelse(weekGames$HomeTeamWon, 1, 0)
  weekGames$logComponentBT<-log(ifelse(weekGames$HomeTeamWon, weekGames$BTHomeWin, 1-weekGames$BTHomeWin))
  weekGames$logComponentTM<-log(ifelse(weekGames$HomeTeamWon, weekGames$TMHomeWin, 1-weekGames$TMHomeWin))
  
  penalties<-c(sum(weekGames$Penalty[weekGames$DidWorse=="Bradley-Terry"]/nrow(weekGames)),
               sum(weekGames$Penalty[weekGames$DidWorse=="Thurstone-Mosteller"])/nrow(weekGames))
  names(penalties)<-c("Bradley-Terry Penalty", "Thurstone-Mosteller Penalty")
  percentHomeWins<-c(mean(weekGames$BTHomeWin), mean(weekGames$TMHomeWin))
  names(percentHomeWins)<-c("Bradely-Terry Home Win Percent", "Thurstone-Mosteller Home Win Percent")
  results<-list(weekGames, table(weekGames$DidBetter), penalties, mean(weekGames$Difference), nrow(weekGames),
                list(BTBrierScore=sum(weekGames$BrierComponentBT^2)/nrow(weekGames),
                     TMBrierScore=sum(weekGames$BrierComponentTM^2)/nrow(weekGames)),
                list(BTLogScore=sum(weekGames$logComponentBT)/nrow(weekGames),
                     TMLogScore=sum(weekGames$logComponentTM)/nrow(weekGames)))
  results
}  

makePerformanceGraph<-function(performance)
{
  graphic<-sapply(performance, FUN=function(vec){vec[1]/(vec[2]+vec[1])})
  plot(1-graphic, type='l', main="More Accuracy Comparison", ylab="Model Favored Proportion",
       xlab="Week", xaxt="n", ylim=c(.1,.9))
  lines(graphic, col="red")
  legend(x="topleft",c("Bradley-Terry", "Thurstone-Mosteller"), col=c("Red", "black"), lty=c(1,1))
  axis(1,at=1:length(graphic),labels=2:(length(graphic)+1))
}

makePenaltyGraph<-function(penalties, lab="Bad Prediction Penalization", yax="Penalty Score", where="bottomleft")
{
  BTPenalties<-sapply(penalties, FUN = function(vec){vec[1]})
  TMPenalties<-sapply(penalties, FUN = function(vec){vec[2]})
  plot(TMPenalties, type='l', lty=2, col="Gray10", main=lab, ylab=yax,
       xlab="Week Predicted", xaxt="n")
  lines(BTPenalties, col="Black")
  axis(1,at=1:length(BTPenalties),labels=2:(length(BTPenalties)+1))
  legend(x=where,c("Bradley-Terry", "Thurstone-Mosteller"), col=c("Black", "Gray10"), lty=c(1,2))
}

makeDifferenceGraph<-function(meanDifferences)
{
  plot(meanDifferences, type="l", main="Average Difference in Prediction (%)", xaxt="n", xlab="Week",
       ylab="% Different", yaxt="n")
  yticks<-seq(floor(min(meanDifferences*100))/100,ceiling(max(meanDifferences*100))/100, by=.001)
  axis(2, at=yticks, label=yticks*100)
  axis(1,at=1:length(meanDifferences),labels=2:(length(meanDifferences)+1))
  
}




performance<-lapply(2:length(stripped2016data), FUN=function(n){stripped2016data[[n]][[4]][[2]]})
makePerformanceGraph(performance)
performance<-lapply(2:length(all2016data), FUN=function(n){all2016data[[n]][[4]][[2]]})
makePerformanceGraph(performance)

brierScores<-lapply(2:(length(stripped2016data)-1), FUN=function(n){
  c(stripped2016data[[n]][[4]][[6]]$BTBrierScore, stripped2016data[[n]][[4]][[6]]$TMBrierScore)})
makePenaltyGraph(brierScores, lab="Brier Scoring 2016 NCAA Football", yax="Brier score")
apply(ldply(brierScores), 2, mean)

logScores<-lapply(2:length(stripped2016data), FUN=function(n){
  c(stripped2016data[[n]][[4]][[7]]$BTLogScore, stripped2016data[[n]][[4]][[7]]$TMLogScore)})
makePenaltyGraph(logScores, lab="Log Scoring", yax="Log score (Closer to zero better)")

sapply(logScores, FUN = which.max)==sapply(penalties, FUN=which.min) #comparing the penalty metric

penalties<-lapply(2:length(stripped2016data), FUN=function(n){stripped2016data[[n]][[4]][[3]]})
makePenaltyGraph(penalties)
rowSums(sapply(1:12, FUN= function(i){penalties[[i]]}))
penaltiesAll<-lapply(2:length(all2016data), FUN=function(n){all2016data[[n]][[4]][[3]]})
makePenaltyGraph(penaltiesAll)
rowSums(sapply(1:12, FUN= function(i){penaltiesAll[[i]]}))


meanDifferences<-sapply(2:length(stripped2016data), FUN=function(n){stripped2016data[[n]][[4]][[4]]})
makeDifferenceGraph(meanDifferences)
#meanDifferences<-sapply(2:length(all2016data), FUN=function(n){all2016data[[n]][[4]][[4]]})
#makeDifferenceGraph(meanDifferences)

performance<-lapply(2:length(stripped2015data), FUN=function(n){stripped2015data[[n]][[4]][[2]]})
makePerformanceGraph(performance)
performance<-lapply(2:length(all2015data), FUN=function(n){all2015data[[n]][[4]][[2]]})
makePerformanceGraph(performance)

brierScores<-lapply(2:(length(stripped2015data)-1), FUN=function(n){
  c(stripped2015data[[n]][[4]][[6]]$BTBrierScore, stripped2015data[[n]][[4]][[6]]$TMBrierScore)})
makePenaltyGraph(brierScores, lab="Brier Scoring 2015 NCAA Football", yax="Brier score", where="topright")
apply(ldply(brierScores), 2, FUN=mean)

logScores<-lapply(2:length(stripped2015data), FUN=function(n){
  c(stripped2015data[[n]][[4]][[7]]$BTLogScore, stripped2015data[[n]][[4]][[7]]$TMLogScore)})
makePenaltyGraph(logScores, lab="Log Scoring", yax="Log score (Closer to zero better)")

penalties<-lapply(2:length(stripped2015data), FUN=function(n){stripped2015data[[n]][[4]][[3]]})
makePenaltyGraph(penalties)

sapply(logScores, FUN = which.max)==sapply(penalties, FUN=which.min) #comparing the penalty metric

corWithEachOther<-sapply(1:15, FUN = function(i) cor(stripped2016data[[i]][[2]]$Strength[order(stripped2016data[[i]][[2]]$Team)],stripped2016data[[i]][[3]]$Strength[order(stripped2016data[[i]][[3]]$Team)], method="spearman"))
plot(corWithEachOther, type='l')
corWithEachOther<-sapply(1:14, FUN = function(i) cor(stripped2015data[[i]][[2]]$Strength[order(stripped2015data[[i]][[2]]$Team)],stripped2015data[[i]][[3]]$Strength[order(stripped2015data[[i]][[3]]$Team)], method="spearman"))
library(plyr)
allWeeks<-lapply(stripped2016data, FUN=function(i)i[[4]][[1]])
allWeeksDF<-ldply(allWeeks)
disagree<-allWeeksDF[(allWeeksDF$BTHomeWin<.5 & allWeeksDF$TMHomeWin>.5)|(allWeeksDF$BTHomeWin>.5 & allWeeksDF$TMHomeWin<.5),]
agree<-allWeeksDF[!((allWeeksDF$BTHomeWin<.5 & allWeeksDF$TMHomeWin>.5)|(allWeeksDF$BTHomeWin>.5 & allWeeksDF$TMHomeWin<.5)),]
mean(ifelse(agree$BTHomeWin>.5, agree$BTHomeWin, 1-agree$BTHomeWin)-ifelse(agree$TMHomeWin>.5, agree$TMHomeWin, 1-agree$TMHomeWin)<0)
mean(ifelse(agree$TMHomeWin>.5, agree$TMHomeWin, 1-agree$TMHomeWin))
all(c(agree$BTHomeWin>.5)==c(agree$TMHomeWin>.5))
nrow(allWeeksDF)