##Simulation set up
numSim <-10 #Change this to 1000

simulation<-list()
simulation[["Type"]]<-"BT"
simulation



generateSeason<-function(type)
{
  if(type=="BT")
  {
    params<-c(2,3,1.5,2.5,1,2,4,.5,1.25,.75)
    func<-pickBTStrength
  }
  else
  {
    params<-c(0,1,-.5,.5,-1,0,2,-1.5,-.75, -1.25)
    func<-pickTMStrength
  }
  
  teams<-data.frame(rep(1:10, each=9), 1:90, rep(0, 90))
  names(teams)<-c("Conference", "Team", "TrueStrength")
  teams$TrueStrength<-func(params[teams$Conference])
  teams
}

save(simulation, "Simulation.RData")