#NCAA week by week

#An analysis file that will pull in football results and run them through LARC Rank

load("NCAAWeek2Configured.RData")
load("BTResultsWeek2.RData")

#ncaadata<-datascrape("NCAAF")
#head(ncaadata)
#system.time(
#football<-dataconfigure(ncaadata)
#)
#head(football)
#system.time(
#bradter<-LARC.Rank(football))
system.time(
Mosteller<-LARC.Rank(football, func = ThurstoneMostellerLARC))
#BradleyTerryRank<-bradter
#week2Configured<-football
#save(week2Configured, file="NCAAWeek2Configured.RData")
#save(BradleyTerryRank, file="BTResultsWeek2.RData")
