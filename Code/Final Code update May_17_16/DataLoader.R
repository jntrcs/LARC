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

save(BTResultsWeek1, BTResultsWeek2, BTResultsWeek3, BTResultsWeek4, TMResultsWeek1, TMResultsWeek2,
     TMResultsWeek3, TMResultsWeek4, week1, week2, week3, week4, predictWeek2, predictWeek3, predictWeek4, latestRaw,
     file="2016FootballData.RData")

#2015 Football