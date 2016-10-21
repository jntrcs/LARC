#This file includes six  functions:

#BradleyTerryLARC: this function is the Bradley-Terry Model. It takes three arguments
# the strengths of the teams, the number of wins for each team, and a matrix that provides
# the number of times each team has played each other.
BradleyTerryLARC <- function(strengths,wins,magnificationfactor=1) {
  PI <- 1
  PIPI <- 1
  W <- vector()
  for (i in 1:length(strengths)) {
    W[i] <- sum(wins[i,])
    PI <- PI*strengths[i]^(W[i]+1)
    if (i<length(strengths)){
    for (j in (i+1):length(strengths)) {
        if ((wins[i,j]+wins[j,i])>0)
          PIPI <- PIPI*(1/(strengths[i]+strengths[j])^(wins[i,j]+wins[j,i]))*magnificationfactor
    }
    }
  }

  return(exp(-sum(strengths))*PI*PIPI)
}

#an example of the function
#BradleyTerryLARC(NBAdf$Strength,NBAdf$WinsVersus)
#the next four lines are to give a comparison of what the preceeding function should return
#y <- 0
#for (i in 1:30) {
#  y <- y + sum(NBAdf$Versus[i,i:30])
#}
#exp(-30)*1*(1/2^y)

#ThurstoneMostellerLARC: this function is the Thurstone-Mosteller Model. It takes three arguments
# the strengths of the teams, the number of wins for each team against each team,
# and a matrix that provides the number of times each team has played each other.
ThurstoneMostellerLARC <- function(strengths,wins,magnificationfactor=1) {
  # First we compute the Prior
  prior <- 1
  for (i in 1: length(strengths)) { prior = prior * dnorm( strengths[i])*magnificationfactor}
  # Now we compute the conditional
  cond <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      if (wins[i,j]!=0)
        cond <- cond*pnorm(strengths[i]-strengths[j])^wins[i,j]*magnificationfactor
    }
  }
  # Now put the two together 
  post = prior * cond
  return(post)
}

#an example of the function
#ThurstoneMostellerLARC(NBAdf$Strength,NBAdf$WinsVersus)

#The following two functions simply generate the probability as dictated by either the
# Bradley-Terry or Thurstone-Mosteller Models

#BTWP (or Bradley-Terry Win Probability) requires at least two arguements (the strengths
# of the two teams) and it returns a statement that tells the probability that the team
# with the first strength will win. The statement is not required however and can be set
# to FALSE by making the third arguement statement = FALSE. Fourth and fith arguements
# can also be enter to provide the names of these teams.
BTWP <- function(team1strength, team2strength, statement = TRUE, team1 = NULL, team2 = NULL) {
  prob <- team1strength/(team1strength + team2strength)
  if (statement == TRUE) {
    if (is.null(team1) == FALSE & is.null(team2) == FALSE) {
      return(cat("The ",team1," have a probability of ",prob," of beating the ",team2,".",sep=""))
    }
    if (is.null(team1) == FALSE & is.null(team2) == TRUE) {
      return(cat("The ",team1," have a probability of ",prob," of winning.",sep=""))
    }
    if (is.null(team1) == TRUE & is.null(team2) == TRUE) {
      return(cat("The probability of the first team winning is ",prob,".",sep=""))
    }
  } else {
    return(prob)
  }
}

#TMWP (or Thurstone-Mosteller Win Probability) works exactly the same as BTWP.
TMWP <- function(team1strength,team2strength, statement = TRUE, team1 = NULL, team2 = NULL) {
  prob <- pnorm(team1strength - team2strength)
  if (statement == TRUE) {
    if (is.null(team1) == FALSE & is.null(team2) == FALSE) {
      return(cat("The ",team1," have a probability of ",prob," of beating the ",team2,".",sep=""))
    }
    if (is.null(team1) == FALSE & is.null(team2) == TRUE) {
      return(cat("The ",team1," have a probability of ",prob," of winning.",sep=""))
    }
    if (is.null(team1) == TRUE & is.null(team2) == TRUE) {
      return(cat("The probability of the first team winning is ",prob,".",sep=""))
    }
  } else {
    return(prob)
  }
}



#This is a version of the first two functions listed in this file, but it will find the 
# magnification factor of either one while calculating the value of the posterior. By using the
# final arguement report you can decide if the magnification factor is also output or not.
LARC.Posterior <- function(df, func = BradleyTerryLARC, mf=1, adj=1, report=FALSE) {
  posterior <- func(df$Strength,df$WinsVersus,mf)
  while (posterior == 0) {
    mf <- mf + adj
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  while (posterior == Inf) {
    mf <- mf - adj
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  if (report==TRUE){
    return(c(posterior,mf))
  } else {
    return(posterior)
  }
}
