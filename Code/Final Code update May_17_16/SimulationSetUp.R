##Simulation function

simulate1<-function()
{
  simulation<-list()
  simulation$teamSchedule<-generateTeamSchedule()
  simulation$BTseasonGames<-generateSeasonResults(simulation$teamSchedule, useBT = TRUE)
  simulation$TMseasonGames<-generateSeasonResults(simulation$teamSchedule, useBT=FALSE)
  strengths<-list()
  for (i in 1:13)
  {
    configured<-dataconfigure(simulation$BTseasonGames,reldate = i)
    if (i!=1)
      configured<-attachMostRecentStrengths(configured, strengths[[i-1]]$BT, strengths[[i-1]]$TM)
    strengths[[i]]<-list()
    strengths[[i]]$BT<-LARC.Rank.Football(configured, func=BTDensity)
    strengths[[i]]$TM<-LARC.Rank.Football(configured, func=TMDensity)
  }
  strengths
}



