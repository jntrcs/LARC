##Season analysis for 2015
load("MasterFunctionFile.RData")
load("2015FootballData.RData")
load("Stripped2015FootballData.RData")
Rcpp::sourceCpp("cppFiles.cpp")

stripped2015data<-list()
raw2015<-datascrape("NCAAF", year=2015)
numGames<-table(c(raw2015$Home, raw2015$Visitor))
numGames<-data.frame(numGames)
takeOut<-as.character(numGames[numGames$Freq<5, 1])
strippedRaw<-raw2015[!(raw2015$Home %in% takeOut | raw2015$Visitor %in% takeOut),]

dates<-seq(as.Date("2015-09-09"), to=as.Date("2015-12-06"), by=7)
dates<-c(dates, as.Date("2016-01-12"))
range<-14

for (i in range)
{
  all2015data[[i]]<-list()
}
for (i in range)
{
  
  all2015data[[i]][[5]]<-c(dates[i-1], dates[i])
  
}

for (i in range){
  stripped2015data[[i]]<-list()
  stripped2015data[[i]][[5]]<-all2015data[[i]][[5]]
}

for (i in range)
{
  all2015data[[i]][[1]]<-dataconfigure(raw2015, reldate = all2015data[[i]][[5]][2])
  stripped2015data[[i]][[1]]<-dataconfigure(strippedRaw, reldate = stripped2015data[[i]][[5]][2])
}

for (i in range)
{
  all2015data[[i]][[1]]<-attachMostRecentStrengths(all2015data[[i]][[1]], all2015data[[i-1]][[2]], all2015data[[i-1]][[3]])
  stripped2015data[[i]][[1]]<-attachMostRecentStrengths(stripped2015data[[i]][[1]], stripped2015data[[i-1]][[2]], stripped2015data[[i-1]][[3]])
}

system.time(
  for (i in range)
  {
    all2015data[[i]][[2]]<-LARC.Rank.Football(all2015data[[i]][[1]])
    stripped2015data[[i]][[2]]<-LARC.Rank.Football(stripped2015data[[i]][[1]])
    all2015data[[i]][[3]]<-LARC.Rank.Football(all2015data[[i]][[1]], func=TMDensity)
    stripped2015data[[i]][[3]]<-LARC.Rank.Football(stripped2015data[[i]][[1]], func=TMDensity)
  }
)

for (i in range)
{
  all2015data[[i]][[4]]<-NCAAFPredictor(all2015data[[i-1]][[2]], all2015data[[i-1]][[3]], raw2015, all2015data[[i]][[5]])
  stripped2015data[[i]][[4]]<-NCAAFPredictor(stripped2015data[[i-1]][[2]], stripped2015data[[i-1]][[3]], strippedRaw, stripped2015data[[i]][[5]])
}

#cbind(all2015data[[range]][[2]], all2015data[[range]][[3]])
#cbind(stripped2015data[[range]][[2]], stripped2015data[[range]][[3]])
#stripped2015data[[range]][[4]]

save(all2015data,raw2015,file="2015FootballData.RData") 
save(stripped2015data, strippedRaw, file="Stripped2015FootballData.RData")
