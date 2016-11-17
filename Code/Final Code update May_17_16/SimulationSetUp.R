##Simulation function

simulate1<-function(useBT)
{
  simulation<-list()
  simulation$TrueStrengthType<-ifelse(useBT, "Bradley-Terry Gamma", "Thurstone-Mosteller Normal")
  simulation$teamSchedule<-generateTeamSchedule(useBT)
  simulation$seasonGames<-generateSeasonResults(simulation$teamSchedule, useBT)
  strengths<-list()
  for (i in 1:13)
  {
    configured<-dataconfigure(simulation$seasonGames,reldate = i)
    if (i!=1)
      configured<-attachMostRecentStrengths(configured, strengths[[i-1]]$BT, strengths[[i-1]]$TM)
    strengths[[i]]<-list()
    strengths[[i]]$BT<-LARC.Rank.Football(configured, func=BTDensity)
    strengths[[i]]$TM<-LARC.Rank.Football(configured, func=TMDensity)
  }
  strengths
}



