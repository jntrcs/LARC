#### NCAAF Predictor
#The purpose of this script is to use the strengths after a given week of football (week 2) to predict the
#next weeks games (week 3). And then to assess which function did so with a higher accuracy.

#TMStrengths and BTStrengths are the Thurston-Mosteller and Bradley-Terry strengths produced by LARC.Rank
#Schedule is the df produced by datascrape
#start date is the beginning of the games we want predictions for (Note, these games must appear in the DF 
#and be already played. Enddate is the last day to predict games for)
TMStrengths<-TMResultsWeek1
BTStrengths<-BTResultsWeek1
schedule<-datascrape("NCAAF")
startdate<-"2016-09-05"
enddate<-"2016-09-11"
NCAAFPredictor<-function(TMStrengths, BTStrengths, schedule, startdate, enddate)
{
  TMStrengths$Team<-as.character(TMStrengths$Team)
  BTStrengths$Team<-as.character(BTStrengths$Team)
  
  weekGames<-schedule[schedule$Date<enddate&schedule$Date>startdate, c(3,5,8,9)]  
  head(weekGames)
  for (i in 1:nrow(weekGames))
  {
    weekGames$HomeTMStength[i]<-TMStrengths$Strength[which(TMStrengths$Team==weekGames$Home[i])]
    weekGames$AwayTMStrength[i]<-TMStrengths$Strength[which(TMStrengths$Team==weekGames$Visitor[i])]
    weekGames$AwayBTStrength[i]<-BTStrengths$Strength[which(BTStrengths$Team==weekGames$Visitor[i])]
    weekGames$HomeBTStrength[i]<-BTStrengths$Strength[which(BTStrengths$Team==weekGames$Home[i])]
  }
  
  weekGames$BTHomeWin<-weekGames$HomeBTStrength/(weekGames$HomeBTStrength+weekGames$AwayBTStrength)
  weekGames$TMHomeWin<-pnorm(weekGames$HomeTMStength-weekGames$AwayTMStrength)
  weekGames$HomeTeamWon<-weekGames$Home==weekGames$Winner
}  