#The purpose of this is to provide a level of abstraction for the confusing way in which the data is stored
getRaw<-function(year=2016)
{
  if (year==2016)
  {
    if (!exists("latestRaw"))
      load("2016FootballData.RData")
    latestRaw
  }
  if (year==2015)
  {
    if(!exists("raw2015"))
      load("2015FootballData.RData")
    raw2015
  }
}

getConfigured<-function(week, year=2016)
{
  if (year==2016)
  {
    if (!exists("all2016data"))
      load("2016FootballData.RData")
    all2016data[[week]][[1]]
  }
  if (year==2015)
   { if(!exists("all2016data"))
        load("2015FootballData.RData")
      all2015data[[week]][[1]]
    }
}

getBTStrengths<-function(week, year=2016)
{
  if (year==2016)
  {
    if (!exists("all2016data"))
      load("2016FootballData.RData")
    all2016data[[week]][[2]]
  }
  if (year==2015)
  { if(!exists("all2016data"))
    load("2015FootballData.RData")
    all2015data[[week]][[2]]
  }
}

getTMStrengths<-function(week, year=2016)
{
  if (year==2016)
  {
    if (!exists("all2016data"))
      load("2016FootballData.RData")
    all2016data[[week]][[3]]
  }
  if (year==2015)
  { if(!exists("all2016data"))
    load("2015FootballData.RData")
    all2015data[[week]][[3]]
  }
}

getStrengths<-function(week, year=2016)
{
  cbind(getBTStrengths(week, year), getTMStrengths(week,year))
}

getPredictions<-function(week, year=2016)
{
  if (year==2016)
  {
    if (!exists("all2016data"))
      load("2016FootballData.RData")
    all2016data[[week]][[4]]
  }
  if (year==2015)
  { if(!exists("all2016data"))
    load("2015FootballData.RData")
    all2015data[[week]][[4]]
  }
}
