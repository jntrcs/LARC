#NCAA week by week

#This script will configure the data and generate the strengths after each week of the football season.
sum(week2Configured[week2Configured$Team=="Hawaii", 4])
week2Configured[week2Configured$Team=="California", 1:3]
BradleyTerryRank[BradleyTerryRank$Team=="California",]
BTResultsWeek3[BTResultsWeek3$Team=="Hawaii",]
BradleyTerryRank[BradleyTerryRank$Team=="Hawaii",]


#WEEK 4
week4<-dataconfigure(datascrape("NCAAF"), reldate = as.Date("2016-09-25"))
save(week4, file="NCAAFWeek4Configured.Rdata")
system.time(
  BTResultsWeek4<-LARC.Rank(week4)
)
system.time(
  TMResultsWeek4<-LARC.Rank(week4, increment = .01, func=ThurstoneMostellerLARC)
)
save(BTResultsWeek4, TMResultsWeek4, file="Week4Results.RData")

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

week2<-dataconfigure(datascrape("NCAAF"), reldate = as.Date("2016-09-11"))
save(week2, file="NCAAFWeek2Configured.Rdata")
system.time(
  BTResultsWeek2<-LARC.Rank(week2)
)
system.time(
  TMResultsWeek2<-LARC.Rank(week2, increment = .01, func=ThurstoneMostellerLARC)
)
save(BTResultsWeek2, TMResultsWeek2, file="Week2Results.RData")

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



 