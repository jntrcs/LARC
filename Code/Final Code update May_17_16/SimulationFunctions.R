#Simulation Functions

generateTeams<-function()
{
  BTparams<-c(2,3,1.5,2.5,1,2,4,.5,1.25,.75)
  
  TMparams<-c(0,1,-.5,.5,-1,0,2,-1.5,-.75, -1.25)
  
  
  teams<-data.frame(rep(1:10, each=9), 1:90)
  names(teams)<-c("Conference", "Team")
  teams$TrueStrengthBT<-pickBTStrength(BTparams[teams$Conference])
  teams$TrueStrengthTM<-pickTMStrength(TMparams[teams$Conference])
  teams
}

generateSchedule<-function()
{
  rbind(generateNonConference(), generateConference())
}

generateNonConference<-function()
{
 
}

generateConference<-function()
{
  
}
