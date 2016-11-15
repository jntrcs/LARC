##Simulation function

simulate1<-function()
  simulation<-list()
  simulation$teamSchedule<-generateTeamSchedule()
  simulation$BTseasonGames<-generateSeasonResults(simulation[[i]]$teamSchedule, useBT = TRUE)
  simulation$TMseasonGames<-generateSeasonResults(simulation[[i]]$teamSchedule, useBT=FALSE)
  for (i in 1:13)
  {
    configured<-dataconfigure()
  }
}



