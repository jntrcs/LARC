#NCAA week by week

load("MasterFunctionFile.RData")
load("2015FootballData.RData")


library(parallel)
numCores<-15
clust <- makeCluster(numCores)

dates<-seq(as.Date("2015-10-10"), to=as.Date("2015-12-06"), by=7)
dates<-c(dates, as.Date("2016-01-12"))
neededFunc<- c("dataconfigure", "raw2015", "all2015data", "dates", "ThurstoneMostellerLARC",   "BradleyTerryLARC", "LARC.Rank", "LARC.Optim", "find.mf")
clusterExport(clust, neededFunc)
system.time(
bradter<-parLapply(clust, dates, function(date){
  LARC.Rank.Football(all2015data[[which(dates==date)+5]][[1]])
})
)
for (i in 6:length(bradter))
{
  all2015data[[i]][[2]]<-bradter[[i-5]]
}
  save(all2015data, raw2015, file="2015FootballData.RData")

stopCluster(clust)

clust<-makeCluster(numCores)
#dates<-seq(as.Date("2015-09-05"), to=as.Date("2015-12-06"), by=7)
#dates<-c(dates, as.Date("2016-01-12"))
clusterExport(clust, neededFunc)

system.time(
thurs<-parLapply(clust, dates, function(date){
  LARC.Rank.Football(all2015data[[which(dates==date)+5]][[1]], func=ThurstoneMostellerLARC, dgt=2)
})
)
for (i in 6:length(thurs))
{
 all2015data[[i]][[3]]<-thurs[[i-5]]
}
  save(all2015data, raw2015, file="2015FootballData.RData")
#system.time(
#all2015data<-parLapply(cl, dates, function(date){
#  list(dataconfigure(raw2015, reldate=date))
#})
#)
stopCluster(clust)



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


##This is going to configure the data so that it contains the strengths for the last week in data configure
#all2015data[[1]][[1]]$BTLast<-rep(1, nrow(all2015data[[1]][[1]]))
#all2015data[[1]][[1]]$TMLast<-rep(0, nrow(all2015data[[1]][[1]]))

#for (i in 2:length(all2015data))
#{
 # for (j in 1:length(all2015data[[i]][[1]]$Team))
  #{
#    all2015data[[i]][[1]]$BTLast[j]<-ifelse(all2015data[[i]][[1]]$Team[j] %in% all2015data[[i-1]][[2]]$Team,
 #                   all2015data[[i-1]][[2]]$Strength[all2015data[[i-1]][[2]]$Team ==all2015data[[i]][[1]]$Team[j]],
  #                                                   1)
   # all2015data[[i]][[1]]$TMLast[j]<-ifelse(all2015data[[i]][[1]]$Team[j] %in% all2015data[[i-1]][[2]]$Team,
     #                                       all2015data[[i-1]][[3]]$Strength[all2015data[[i-1]][[3]]$Team ==all2015data[[i]][[1]]$Team[j]],
    #                                        0)
#  }
#}