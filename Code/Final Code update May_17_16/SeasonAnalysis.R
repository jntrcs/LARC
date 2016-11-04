##Season analysis 

dates<-seq(as.Date("2016-09-04"), to=Sys.Date(), by=7)
for (i in 5:9)
{
  
  all2016data[[i]][[5]]<-c(dates[i-1], dates[i])
}

for (i in 1:9){
  stripped2016data[[i]]<-list()
  stripped2016data[[i]][[5]]<-all2016data[[i]][[5]]
}

for (i in 1:9)
{
  all2016data[[i]][[1]]<-dataconfigure(latestRaw, reldate = all2016data[[i]][[5]][2])
  stripped2016data[[i]][[1]]<-dataconfigure(strippedRaw, reldate = stripped2016data[[i]][[5]][2])
}

system.time(
for (i in 1:9)
{
  #all2016data[[i]][[2]]<-LARC.Rank(all2016data[[i]][[1]])
  stripped2016data[[i]][[2]]<-LARC.Rank(stripped2016data[[i]][[1]])
  #all2016data[[i]][[3]]<-LARC.Rank(all2016data[[i]][[1]], func=TMDensity)
  stripped2016data[[i]][[3]]<-LARC.Rank(stripped2016data[[i]][[1]], func=TMDensity)
}
)

for (i in 2:9)
{
  #all2016data[[i]][[4]]<-NCAAFPredictor(all2016data[[i-1]][[2]], all2016data[[i-1]][[3]], latestRaw, all2016data[[i]][[5]])
  stripped2016data[[i]][[4]]<-NCAAFPredictor(stripped2016data[[i-1]][[2]], stripped2016data[[i-1]][[3]], strippedRaw, stripped2016data[[i]][[5]])
}
