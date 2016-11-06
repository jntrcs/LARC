##Simulation set up
numSim <-10 #Change this to 1000

simulation<-list()

for (i in 1:numSim)
{
  ##idea: change all of these to not store in i so that it doesn't save all info for 1000 sims
 simulation[[i]]<-list()
  simulation[[i]]$Teams<-generateTeams()
  simulation[[i]]$Schedule<-generateSchedule()
  
}


generateTeams()
save(simulation, "Simulation.RData")