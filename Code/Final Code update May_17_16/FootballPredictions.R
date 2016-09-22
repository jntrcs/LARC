#### NCAAF Predictor
#The purpose of this script is to use the strengths after a given week of football (week 2) to predict the
#next weeks games (week 3). And then to assess which function did so with a higher accuracy.

#TMStrengths and BTStrengths are the Thurston-Mosteller and Bradley-Terry strengths produced by LARC.Rank
#Schedule is the df produced by datascrape
#start date is the beginning of the games we want predictions for (Note, these games must appear in the DF 
#and be already played. Enddate is the last day to predict games for)

#The return value is a list. The first item is a dataframe containing all games predicted and info on predictions
#The second value is a table of which model more confidently predicted the winner
#The third value is the penalties assessed. Remember the penalty is computed thus: if the model's prediction
#was further from the actual result (1 for win, 0 for loss), then that team is assessed a penalty of the 
#distance from their prediction to the correct answer. This gives us the desirable property that if Team A
#beat Team B 95 out of 100 times, and model A predicted team A at 95% and Model B at 99%, 
#Model B would be penalized more for it's 5 misses then A for it's 95 misses.
TMStrengths<-MTNCAAFweek2
BTStrengths<-BTStrengths
schedule<-datascrape("NCAAF")
startdate<-"2016-09-11" #Week 2
enddate<-"2016-09-18"
NCAAFPredictor<-function(TMStrengths, BTStrengths, schedule, startdate, enddate)
{
  TMStrengths$Team<-as.character(TMStrengths$Team)
  BTStrengths$Team<-as.character(BTStrengths$Team)
  startdate<-as.Date(startdate)
  enddate<-as.Date(enddate)
  
  
  weekGames<-schedule[schedule$Date<enddate&schedule$Date>startdate, c(3,5,8,9)]  
  for (i in 1:nrow(weekGames))
  {
    #The messy if elses are to handle the case where they play a team that has no previously estimated strength
    weekGames$HomeTMStength[i]<-ifelse(weekGames$Home[i]%in%TMStrengths$Team,TMStrengths$Strength[which(TMStrengths$Team==weekGames$Home[i])],0)
    weekGames$AwayTMStrength[i]<-ifelse(weekGames$Visitor[i]%in%TMStrengths$Team,TMStrengths$Strength[which(TMStrengths$Team==weekGames$Visitor[i])],0)
    weekGames$AwayBTStrength[i]<-ifelse(weekGames$Home[i]%in%BTStrengths$Team,BTStrengths$Strength[which(BTStrengths$Team==weekGames$Visitor[i])],1)
    weekGames$HomeBTStrength[i]<-ifelse(weekGames$Visitor[i]%in%BTStrengths$Team,BTStrengths$Strength[which(BTStrengths$Team==weekGames$Home[i])],1)
  }
  #Right now there is an error in the datascrape where it lists the winner as the away team. 
  #When I get around to fixing this will work accurately
  weekGames$BTHomeWin<-weekGames$HomeBTStrength/(weekGames$HomeBTStrength+weekGames$AwayBTStrength)
  weekGames$TMHomeWin<-pnorm(weekGames$HomeTMStength-weekGames$AwayTMStrength)
  weekGames$HomeTeamWon<-weekGames$Home==weekGames$Winner
  head(weekGames)
  #This line looks at which prediction was closer to the actual value of who won and who lost
  weekGames$DidBetter<-ifelse(abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$BTHomeWin)<abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$TMHomeWin), "Bradley-Terry", "Thurstone-Mosteller")
  weekGames$DidBetter[weekGames$BTHomeWin == weekGames$TMHomeWin]<-"Tie"
  weekGames$DidWorse<-ifelse(weekGames$DidBetter=="Bradley-Terry", "Thurstone-Mosteller", "Bradley-Terry")
  weekGames$DidWorse[weekGames$DidBetter=="Tie"]<-"Tie"
  weekGames$Penalty <- ifelse(weekGames$DidWorse =="Bradley-Terry", abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$BTHomeWin), abs(ifelse(weekGames$HomeTeamWon, 1, 0)-weekGames$TMHomeWin))
  
  penalties<-c(sum(weekGames$Penalty[weekGames$DidWorse=="Bradley-Terry"]),
  sum(weekGames$Penalty[weekGames$DidWorse=="Thurstone-Mosteller"]))
  names(penalties)<-c("Bradley-Terry Penalty", "Thurstone-Mosteller Penalty")
  penalties
  results<-list(weekGames, table(weekGames$DidBetter), penalties)
  results
}  

predictWeek2<-NCAAFPredictor(TMResultsWeek1, BTResultsWeek1, datascrape("NCAAF"), "2016-09-05", "2016-09-11")
predictWeek3<-NCAAFPredictor(MTNCAAFweek2, BTStrengths, datascrape("NCAAF"), "2016-09-11", "2016-09-18")
predictWeek3[[3]]
