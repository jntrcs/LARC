##2016 Football 
load("NCAAFWeek1Configured.Rdata")
load("NCAAFWeek2Configured.Rdata")
load("NCAAFWeek3Configured.Rdata")
load("NCAAFWeek4Configured.Rdata")
load("Week1Results.RData")
load("Week2Results.RData")
load("Week3Results.RData")
load("Week4Results.RData")
load("WeeklyPredictions.RData")
load("LatestNCAAF.RData")

latestRaw<-datascrape("NCAAF")

save(BTResultsWeek1, BTResultsWeek2, BTResultsWeek3, BTResultsWeek4, BTResultsWeek5,
     TMResultsWeek1, TMResultsWeek2, TMResultsWeek3, TMResultsWeek4, TMResultsWeek5,
     week1, week2, week3, week4,week5 predictWeek2, predictWeek3, predictWeek4, latestRaw,
     file="2016FootballData.RData") 
#c
#2015 Football