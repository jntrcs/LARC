##Simulation set up
numSim <-10 #Change this to 1000

simulation<-list()

for (i in 1:numSim)
{
  ##idea: change all of these to not store in i so that it doesn't save all info for 1000 sims
 simulation[[i]]<-list()
  simulation[[i]]$teamSchedule<-generateTeamSchedule()
  simulation[[i]]$BTseasonGames<-generateSeasonResults(simulation[[i]]$teamSchedule, useBT = TRUE)
  simulation[[i]]$TMseasonGames<-generateSeasonResults(simulation[[i]]$teamSchedule, useBT=FALSE)
  
}

a<-dataconfigure(simulation[[i]]$BTseasonGames, reldate= 13)

save(simulation, file="Simulation.RData")

