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
  
  penalties<-c(sum(weekGames$Penalty[weekGames$DidWorse=="Bradley-Terry"]),
  sum(weekGames$Penalty[weekGames$DidWorse=="Thurstone-Mosteller"]))
  names(penalties)<-c("Bradley-Terry Penalty", "Thurstone-Mosteller Penalty")
  percentHomeWins<-c(mean(weekGames$BTHomeWin), mean(weekGames$TMHomeWin))
  names(percentHomeWins)<-c("Bradely-Terry Home Win Percent", "Thurstone-Mosteller Home Win Percent")
  results<-list(weekGames, table(weekGames$DidBetter), penalties, mean(weekGames$Difference), nrow(weekGames))
  results
}  


temp<-lapply(2:17, FUN=function(i) NCAAFPredictor(all2015data[[i-1]][[2]],all2015data[[i-1]][[3]],raw2015,all2015data[[i]][[5]]))
for (i in 2:17)
{
  all2015data[[i]][[4]]<-temp[[i-1]]
}

performance<-lapply(2:16, FUN=function(n){all2015data[[n]][[4]][[2]]})
makePerformanceGraph(performance)

penalties<-lapply(2:13, FUN=function(n){all2015data[[n]][[4]][[3]]/all2015data[[n]][[4]][[5]]})
makePenaltyGraph(penalties)

meanDifferences<-sapply(2:16, FUN=function(n){all2015data[[n]][[4]][[4]]})
makeDifferenceGraph(meanDifferences)


makePerformanceGraph<-function(performance)
{
  graphic<-sapply(performance, FUN=function(vec){vec[1]/(vec[2]+vec[1])})
  plot(graphic, type='l', main="Bradley-Terry 'Win' Percentage", ylab="Percent BT Model Favored",
     xlab="Week", xaxt="n")
  axis(1,at=1:length(graphic),labels=2:(length(graphic)+1))
}

performance<-lapply(2:length(all2016data), FUN=function(n){all2016data[[n]][[4]][[2]]})
makePerformanceGraph(performance)


makePenaltyGraph<-function(penatlies)
{
BTPenalties<-sapply(penalties, FUN = function(vec){vec[1]})
TMPenalties<-sapply(penalties, FUN = function(vec){vec[2]})
plot(TMPenalties, type='l', lty=2, col="Blue", main="Bad Prediction Penalization", ylab="Penalty Score",
     xlab="Week Predicted", xaxt="n")
lines(BTPenalties, col="Red")
axis(1,at=1:length(BTPenalties),labels=2:(length(BTPenalties)+1))
legend(x="bottomleft",c("Bradley-Terry", "Thurstone-Mosteller"), col=c("Red", "Blue"), lty=c(1,2))
}

penalties<-lapply(2:length(all2016data), FUN=function(n){all2016data[[n]][[4]][[3]]})
makePenaltyGraph(penalties)

makeDifferenceGraph<-function(meanDifferences)
{
  plot(meanDifferences, type="l", main="Average Difference in Prediction (%)", xaxt="n", xlab="Week",
       ylab="% Different", yaxt="n")
  yticks<-seq(floor(min(meanDifferences*100))/100,ceiling(max(meanDifferences*100))/100, by=.001)
  axis(2, at=yticks, label=yticks*100)
  axis(1,at=1:length(meanDifferences),labels=2:(length(meanDifferences)+1))

}
meanDifferences<-sapply(2:length(all2016data), FUN=function(n){all2016data[[n]][[4]][[4]]})
makeDifferenceGraph(meanDifferences)

games<-sapply(2:17, FUN=function(i){all2015data[[i]][[4]][[5]]})
plot(games)
games
all2015data[[2]][[4]][[5]]

