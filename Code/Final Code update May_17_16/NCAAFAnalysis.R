#NCAA week by week

#An analysis file that will pull in football results and run them through LARC Rank



#WEEK 3
week3<-dataconfigure(datascrape("NCAAF"), reldate = as.Date("2016-09-18"))
save(week3, file="NCAAFWeek3Configured.Rdata")
system.time(
  BTResultsWeek3<-LARC.Rank(week3)
)
system.time(
  TMResultsWeek3<-LARC.Rank(week3, increment = .01, func=ThurstoneMostellerLARC)
)
save(BTResultsWeek3, TMResultsWeek3, file="Week3Results.RData")

#WEEK 2
#load("BTResultsWeek2.RData")
#load("MTResultsWeek2.RData")
#load("NCAAWeek2Configured.RData")


#ncaadata<-datascrape("NCAAF")
#head(ncaadata)
#system.time(
#football<-dataconfigure(ncaadata)
#)
#head(football)
#system.time(
#BTWeek2<-LARC.Rank(week2Configured)
#system.time(
#TMWeek2<-LARC.Rank(week2Configured, increment=.01, func = ThurstoneMostellerLARC) #11 minutes at .01 accuracy
#save(BRWeek2, TMWeek2, file="Week2Results.RData")
#week2Configured<-football
#save(week2Configured, file="NCAAWeek2Configured.RData")
#save(BradleyTerryRank, file="BTResultsWeek2.RData")
#Mosteller
#MTNCAAFweek2<-Mosteller
#save(MTNCAAFweek2, file="MTResultsWeek2.RData")
#cbind(MTNCAAFweek2, BradleyTerryRank)

#WEEK 1
#load("NCAAWeek1Configured.RData")
#load("Week1Results.RData")

week1<-dataconfigure(datascrape("NCAAF"), reldate = as.Date("2016-09-04"))
save(week1, file="NCAAFWeek1Configured.Rdata")
system.time(
BTResultsWeek1<-LARC.Rank(week1)
)
system.time(
TMResultsWeek1<-LARC.Rank(week1, increment = .01, func=ThurstoneMostellerLARC)
)
save(BTResultsWeek1, TMResultsWeek1, file="Week1Results.RData")
#cbind(BTResultsWeek1, TMResultsWeek1)

date<-"2016-09-19"
weekNumber<-3
buildWeek<-function(weekNumber, date) #Takes a date in form "YYYY-MM-DD" and scrapes the data, configures, and runs both ranking systems on it. Saves the results
  #this is to be called at the start of each week for the last week
{
  if (file.exists("FootballData.RData"))
    load("FootballData.RData")
  if (!exists("footballdata"))
    footballdata<-list()
  if (is.null(footballdata[[weekNumber]]))
  {
    footballdata[[weekNumber]]<-dataconfigure(datascrape("NCAAF"), reldate=date)
    save(footballdata, file="FootballData.RData")
  }
  
}

 