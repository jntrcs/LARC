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

generateSchedule<-function(teams)
{
  
  cbind(generateNonConference(teams), generateConference(teams))
}

generateNonConference<-function(teams)
{
  schedule<-data.frame(teams$Conference, teams$Team, matrix(0, nrow=nrow(teams), ncol=4))
  names(schedule)<-c("Conference", "Team", "Week1", "Week2", "Week3", "Week4")
  for (i in 1:4)
  {
    for (j in 1:nrow(schedule))
    {
      if (schedule[j,i+2]==0) #If it's already filled, skip it.
      {
        opposingTeam<-0
        while((opposingTeam %in% schedule[j, 3:6])){
          opposingTeam<-sample(1:sum(schedule[,i+2]==0 & schedule$Conference[j]!=schedule$Conference), 1) #This will sample from how many teams there are left to be filled
          if (opposingTeam>=(schedule$Conference[j]-1)*9) #This prevents a team from playing within their own conference
            opposingTeam<-opposingTeam+9
        } 
        schedule[j, i+2]<-opposingTeam
        schedule[opposingTeam, i+2]<-j
      }
    }
  }
  schedule
}

generateConference<-function()
{
  
}
