#Simulation Functions

generateTeams<-function(useBT)
{
  if(useBT) {
      params<-c(2,3,1.5,2.5,1,2,4,.5,1.25,.75)
      func <-pickBTStrength
  }
  else{
    params<-c(0,1,-.5,.5,-1,0,2,-1.5,-.75, -1.25)
    func<-pickTMStrength
  }
  
  teams<-data.frame(rep(1:10, each=9), 1:90)
  names(teams)<-c("Conference", "Team")
  teams$TrueStrength<-func(params[teams$Conference])
  teams
}

generateTeamSchedule<-function(useBT)
{
  teams<-generateTeams(useBT)
  generateSchedule(teams)
}

generateSchedule<-function(teams)
{
  
  cbind(generateNonConference(teams), generateConference(teams))
}

generateNonConference<-function(teams)
{
  schedule<-data.frame(teams, matrix(0, nrow=nrow(teams), ncol=4))
  names(schedule)<-c("Conference", "Team", "TrueStrength",  "Week1", "Week2", "Week3", "Week4")
  w1index<-which(names(schedule)=="Week1")
  lastIndex<-  ncol(schedule)            
  for (i in w1index:lastIndex)
  {
    randomize<-sample(1:90)
    for (j in 1:90)
    {
      if (schedule[j,i]==0){
      possibilities<-randomize[which(schedule$Conference[randomize]!=schedule$Conference[j] & 
                                       !randomize %in% schedule[j, w1index:lastIndex]
                                     & schedule[randomize,i]==0)]
      if (length(possibilities)>0)
      {
        opposingTeam<-possibilities[1]
        schedule[j,i]<-opposingTeam
        schedule[opposingTeam, i]<-j
      }
      else
      {
       possibilities <-sample((1:90)[schedule$Conference[j]!=schedule$Conference&!(1:90 %in% schedule[j, w1index:lastIndex])])
       replaceTeam<-sample(possibilities, 1)
       otherTeam<-schedule[replaceTeam,i]
       schedule[j,i]<-replaceTeam
       schedule[replaceTeam,i]<-j
       lastTeam<-which(schedule[,i]==0)[1]
       schedule[otherTeam, i]<-lastTeam
       schedule[lastTeam,i]<-otherTeam
   
      }
      }
    }
  }
  schedule
}

generateConference<-function(teams)
{
  schedule<-data.frame(teams$Conference, teams$Team, matrix(0, nrow=nrow(teams), ncol=9))
  names(schedule)<-c("Conference", "Team", rep(paste("Week",5:13,sep="")))
  arrangement<-matrix(c(9,8,7,6,0,4,3,2,1,
                       4,3,2,1,9,8,0,6,5,
                       8,7,6,5,4,3,2,1,0,
                       3,0,1,9,8,7,6,5,4,
                       7,6,5,0,3,2,1,9,8,
                       2,1,9,8,7,0,5,4,3,
                       6,5,4,3,2,1,9,0,7,
                       0,9,8,7,6,5,4,3,2,
                       5,4,0,2,1,9,8,7,6), nrow=9, ncol=9)
  
  for (i in 1:length(unique(schedule$Conference)))
  {
    weekOrder<-sample(1:9)
    schedule[schedule$Conference==i, 3:11]<-arrangement[,weekOrder]
    zeroes<-which(as.vector(arrangement[,weekOrder])==0)
    schedule[schedule$Conference==i, 3:11]<-schedule[schedule$Conference==i, 3:11]+(sum(schedule$Conference==i)*(i-1))
    m<-as.vector(as.matrix(schedule[schedule$Conference==i, 3:11]))
    m[zeroes]<-0
    schedule[schedule$Conference==i, 3:11]<-matrix(m, nrow=9,ncol=9)
  }

  schedule[,3:11]
}

generateSeasonResults<-function(season, useBT)
{
  numGames<-nrow(season)/2*12
  seasonGames<-data.frame(matrix(0, nrow=numGames, 5))
  names(seasonGames)<-c("Date", "Home", "Visitor", "Winner", "HomeWinPerecent")
  game<-0
  week0Index<-which(names(season)=="Week1")-1
  for (i in 1:13)
  {
    for (j in 1:nrow(season))
    {
      if (season[j, i+week0Index]>j)
      {
        game<-game+1
        seasonGames$Date[game]<-i
        seasonGames$Home[game]<-j
        visitor<-season[j, i+week0Index]
        seasonGames$Visitor[game]<-visitor
        seasonGames$HomeWinPerecent[game]<-ifelse(useBT, season$TrueStrength[j]/(season$TrueStrength[j]+season$TrueStrength[visitor]),
                                          pnorm(season$TrueStrength[j]-season$TrueStrength[visitor]))
        seasonGames$Winner[game]<-ifelse(rbinom(1,1,seasonGames$HomeWinPerecent[game]), j, visitor)
      }
    }
  }
    seasonGames
}

a<-generateSeasonResults(season, TRUE)
b<-generateSeasonResults(season, FALSE)
hist(b$HomeWinPerecent)
hist(a$HomeWinPerecent)
mean(a$Visitor)
mean(b$Home)
mean(b$Winner)
