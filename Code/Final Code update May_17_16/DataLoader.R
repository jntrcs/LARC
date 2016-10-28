##2016 Football 

latestRaw<-datascrape("NCAAF")


save(all2016data,latestRaw,file="2016FootballData.RData") 

#This exists because the supercomputer could quite easily overwrite my existing work and I want to 
#keep this easily available, also will be able to use for before and after comparisons
all2016dataCopy<-all2016data
latestRawCopy<-latestRaw
save(all2016data, latestRaw, file="2016copy.RData")

#The structure will be as follows: each item in the first list corresponds to what week it relates too. 
#eg. all2016data[[1]] refers to week ones data
#Each item is a sublist containing these items:
#1: the dataConfiguration for that week--Dataframe with huge matrix
#2: The calculated Bradley Terry strengths for that week--DF
#3: The calculated Thurstone Mosteller strengths for that week--DF
#4: The prediction information for the games for that week (which is itself a list)
#5: The starting date and ending date of that week (note: bowl games are lumped together so not every week is 7 days)
#eg. all2016data[[3]][[2]] is the TM and BT strengths for week 3
dates2016<-c(seq(as.Date("2016-09-04"), as.Date("2016-10-23"), by=7))

all2016data[[1]][[5]]<-c(as.Date("2016-08-20"),dates2016[1])
names(all2016data[[1]][[5]])<-c("Start date", "End Date")

for (i in 8)
{
  all2016data[[i]][[5]]<-c(dates2016[i-1],dates2016[i])
  names(all2016data[[i]][[5]])<-c("Start date", "End Date")
}

dates<-seq(as.Date("2015-09-06"), to=as.Date("2015-12-20"), by=7)
dates<-c(dates, as.Date("2016-01-12"))
all2015data[[1]][[5]]<-c(as.Date("2015-08-20"), dates[1])
for (i in 2:17)
{
  all2015data[[i]][[5]]<-c(dates[i-1], dates[i])
  names(all2015data[[i]][[5]])<-c("Start Date", "End Date")
}

#Configure
all2016data[[8]]<-list()
all2016data[[8]][[5]]
all2016data[[8]][[1]]<-dataconfigure(latestRaw, reldate=all2016data[[8]][[5]][2])
all2016data[[8]][[2]]<-LARC.Rank.Football(all2016data[[8]][[1]])
all2016data[[8]][[3]]<-LARC.Rank.Football(all2016data[[8]][[1]], func=ThurstoneMostellerLARC

#2015 Football
raw2015<-datascrape("NCAAF", year=2015)
save(all2015data, raw2015, file="2015FootballData.RData")

all2015dataCopy<-all2015data
raw2015Copy<-raw2015
save(all2015dataCopy, raw2015Copy, file="2015Copy.Rdata")

rm(all2015dataCopy, all2016dataCopy, raw2015Copy, latestRawCopy)


