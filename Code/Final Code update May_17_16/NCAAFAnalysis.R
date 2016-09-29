#NCAA week by week

load("LatestNCAAF.RData")

#WEEK 4
week4<-dataconfigure(latestRaw, reldate = as.Date("2016-09-25"))
save(week4, file="NCAAFWeek4Configured.Rdata")
print(head(week4[1:2,]))
