#NCAA week by week

#An analysis file that will pull in football results and run them through LARC Rank

load("NCAAWeek2Configured.RData")
load("BTResultsWeek2.RData")
load("'MTResultsWeek2.RData")

#ncaadata<-datascrape("NCAAF")
#head(ncaadata)
#system.time(
#football<-dataconfigure(ncaadata)
#)
#head(football)
#system.time(
#bradter<-LARC.Rank(football))
system.time(
Mosteller<-LARC.Rank(week2Configured, increment=.01, func = ThurstoneMostellerLARC)) #11 minutes at .01 accuracy
#BradleyTerryRank<-bradter
#week2Configured<-football
#save(week2Configured, file="NCAAWeek2Configured.RData")
#save(BradleyTerryRank, file="BTResultsWeek2.RData")
#Mosteller
#MTNCAAFweek2<-Mosteller
#save(MTNCAAFweek2, file="MTResultsWeek2.RData")
cbind(MTNCAAFweek2, BradleyTerryRank)
