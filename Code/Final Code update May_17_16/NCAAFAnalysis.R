#NCAA week by week

load("MasterFunctionFile.RData")
load("LatestNCAAF.RData")

#Week 5
week5<-dataconfigure(latestRaw, relDate=as.Date("2016-10-02"))
save(week5, file="NCAAFWeek5Configured.RData")
system.time(
  BTResultsWeek5<-LARC.Rank(week5)
)
system.time(
  TMResultsWeek5<-LARC.Rank(week5, increment = .01, func=ThurstoneMostellerLARC)
)
save(BTResultsWeek5, TMResultsWeek5, file="Week5Results.RData")

