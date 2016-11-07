##Season analysis 
latestRaw<-datascrape("NCAAF")
numGames<-table(c(latestRaw$Home, latestRaw$Visitor))
numGames<-data.frame(numGames)
takeOut<-as.character(numGames[numGames$Freq<5, 1])
strippedRaw<-latestRaw[!(latestRaw$Home %in% takeOut | latestRaw$Visitor %in% takeOut),]

dates<-seq(as.Date("2016-09-04"), to=Sys.Date(), by=7)

for (i in 10)
{
  all2016data[[i]]<-list()
}
for (i in 10)
{
  
  all2016data[[i]][[5]]<-c(dates[i-1], dates[i])
  
}

for (i in 10){
  stripped2016data[[i]]<-list()
  stripped2016data[[i]][[5]]<-all2016data[[i]][[5]]
}

for (i in 10)
{
  all2016data[[i]][[1]]<-dataconfigure(latestRaw, reldate = all2016data[[i]][[5]][2])
  stripped2016data[[i]][[1]]<-dataconfigure(strippedRaw, reldate = stripped2016data[[i]][[5]][2])
}

for (i in 10)
{
  all2016data[[i]][[1]]<-attachMostRecentStrengths(all2016data[[i]][[1]], all2016data[[i-1]][[2]], all2016data[[i-1]][[3]])
  stripped2016data[[i]][[1]]<-attachMostRecentStrengths(stripped2016data[[i]][[1]], stripped2016data[[i-1]][[2]], stripped2016data[[i-1]][[3]])
}

system.time(
for (i in 10)
{
  all2016data[[i]][[2]]<-LARC.Rank.Football(all2016data[[i]][[1]])
  stripped2016data[[i]][[2]]<-LARC.Rank.Football(stripped2016data[[i]][[1]])
  all2016data[[i]][[3]]<-LARC.Rank.Football(all2016data[[i]][[1]], func=TMDensity)
  stripped2016data[[i]][[3]]<-LARC.Rank.Football(stripped2016data[[i]][[1]], func=TMDensity)
}
)

for (i in 10)
{
  all2016data[[i]][[4]]<-NCAAFPredictor(all2016data[[i-1]][[2]], all2016data[[i-1]][[3]], latestRaw, all2016data[[i]][[5]])
  stripped2016data[[i]][[4]]<-NCAAFPredictor(stripped2016data[[i-1]][[2]], stripped2016data[[i-1]][[3]], strippedRaw, stripped2016data[[i]][[5]])
}

cbind(all2016data[[10]][[2]], all2016data[[10]][[3]])
cbind(stripped2016data[[10]][[2]], stripped2016data[[10]][[3]])

save(all2016data,latestRaw,file="2016FootballData.RData") 
save(stripped2016data, strippedRaw, file="Stripped2016FootballData.RData")
