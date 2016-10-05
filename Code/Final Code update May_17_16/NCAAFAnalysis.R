#NCAA week by week

load("MasterFunctionFile.RData")
load("2015FootballData.RData")


library(parallel)
numCores<-15
cl <- makeCluster(numCores)

dates<-seq(as.Date("2015-09-05"), to=as.Date("2015-12-06"), by=7)
dates<-c(dates, as.Date("2016-01-12"))
clusterExport(cl, c("dataconfigure", "raw2015", "all2015data", "dates", "ThurstoneMostellerLARC", 
                    "BradleyTerryLARC", "LARC.Rank", "LARC.Optim"))
system.time(
bradter<-parLapply(c1, dates, function(date){
  LARC.Rank(all2015data[[which(dates==date)]][[1]])
})
)
for (i in 1:length(bradter))
{
  all2015data[[i]][[2]]<-bradter[[i]]
}
save(all2015data, raw2015, file="2015FootballData.RData")


system.time(
thurs<-parLapply(c1, dates, function(date){
  LARC.Rank(all2015data[[which(dates==date)]][[1]], func=ThurstoneMostellerLARC, dgt=2)
})
)
for (i in 1:length(thurs))
{
  all2015data[[i]][[3]]<-thurs[[i]]
}
#system.time(
#all2015data<-parLapply(cl, dates, function(date){
#  list(dataconfigure(raw2015, reldate=date))
#})
#)
stopCluster(cl)

save(all2015data, raw2015, file="2015FootballData.RData")

#Week 5
#print("Games analyzed")
#print(nrow(latestRaw)))
#stopifnot(nrow(latestRaw)==350)	
#week5<-dataconfigure(latestRaw, reldate=as.Date("2016-10-02"))
#save(week5, file="NCAAFWeek5Configured.RData")
#system.time(
#  BTResultsWeek5<-LARC.Rank(week5)
#)
#system.time(
#  TMResultsWeek5<-LARC.Rank(week5, increment = .01, func=ThurstoneMostellerLARC)
#)
#save(BTResultsWeek5, TMResultsWeek5, file="Week5Results.RData")

#WEEK 4
#week4<-dataconfigure(latestRaw, reldate = as.Date("2016-09-25"))
#save(week4, file="NCAAFWeek4Configured.Rdata")

#system.time(
#  BTResultsWeek4<-LARC.Rank(week4)
#)
#system.time(
 # TMResultsWeek4<-LARC.Rank(week4, increment = .01, func=ThurstoneMostellerLARC)
#)
#save(BTResultsWeek4, TMResultsWeek4, file="Week4Results.RData")

#WEEK 3
#week3<-dataconfigure(latestRaw, reldate = as.Date("2016-09-18"))
#save(week3, file="NCAAFWeek3Configured.Rdata")
#system.time(
#  BTResultsWeek3<-LARC.Rank(week3)
#)
#system.time(
 # TMResultsWeek3<-LARC.Rank(week3, increment = .01, func=ThurstoneMostellerLARC)
#)
#save(BTResultsWeek3, TMResultsWeek3, file="Week3Results.RData")

#WEEK 2

#week2<-dataconfigure(latestRaw, reldate = as.Date("2016-09-11"))
#save(week2, file="NCAAFWeek2Configured.Rdata")
#system.time(
#  BTResultsWeek2<-LARC.Rank(week2)
#)
#system.time(
#  TMResultsWeek2<-LARC.Rank(week2, increment = .01, func=ThurstoneMostellerLARC)
#)
#save(BTResultsWeek2, TMResultsWeek2, file="Week2Results.RData")

#WEEK 1
#load("NCAAWeek1Configured.RData")
#load("Week1Results.RData")

#week1<-dataconfigure(latestRaw, reldate = as.Date("2016-09-04"))
#save(week1, file="NCAAFWeek1Configured.Rdata")
#system.time(
#  BTResultsWeek1<-LARC.Rank(week1)
#)
#system.time(
 # TMResultsWeek1<-LARC.Rank(week1, increment = .01, func=ThurstoneMostellerLARC)
#)
#save(BTResultsWeek1, TMResultsWeek1, file="Week1Results.RData")
#cbind(BTResultsWeek1, TMResultsWeek1)


