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
#eg. all2016data[[3]][[2]] is the TM and BT strengths for week 3
all2016data<-list(list(week1, BTResultsWeek1, TMResultsWeek1), 
                     list(week2, BTResultsWeek2, TMResultsWeek2, predictWeek2),
                     list(week3, BTResultsWeek3, TMResultsWeek3, predictWeek3),
                     list(week4, BTResultsWeek4, TMResultsWeek4, predictWeek4),
                     list(week5, BTResultsWeek5, TMResultsWeek5))

#2015 Football
all2015data<-list()
raw2015<-datascrape("NCAAF", year=2015)
save(all2015data, raw2015, file="2015FootballData.RData")

all2015dataCopy<-all2015data
raw2015Copy<-raw2015
save(all2015dataCopy, raw2015Copy, file="2015Copy.Rdata")

rm(all2015dataCopy, all2016dataCopy, raw2015Copy, latestRawCopy)
