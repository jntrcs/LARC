#install.packages("optimx")
#This file includes three  functions:

# BradleyTerryLARC: this function is the Bradley-Terry Model. It takes three arguments
#the strengths of the teams, the number of wins for each team, and a matrix that provides
#the number of times each team has played each other.
BradleyTerryLARC <- function(strengths,wins,magnificationfactor=1) {
  PI <- 1
  PIPI <- 1
  W <- vector()
  x <- 0
  for (i in 1:length(strengths)) {
    W[i] <- sum(wins[i,])
    PI <- PI*strengths[i]^(W[i]+1)
    for (j in (i+1):length(strengths)) {
      if (j < length(strengths)+1) {
        x <- x + 1
        PIPI <- PIPI*(1/(strengths[i]+strengths[j])^(wins[i,j]+wins[j,i]))*magnificationfactor
      }
    }
  }
  return(exp(-sum(strengths))*PI*PIPI)
}

#an example of the function
BradleyTerryLARC(NBAdf$Strength,NBAdf$WinsVersus)
BradleyTerryLARC(NHLdf$Strength,NHLdf$WinsVersus)
#the next four lines are to give a comparison of what the preceeding function should return
y <- 0
for (i in 1:30) {
  y <- y + sum(NBAdf$Versus[i,i:30])
}
exp(-30)*1*(1/2^y)

# MostellerLARC: this function is the Mosteller Model. It takes three arguments
#the strengths of the teams, the number of wins for each team against each team,
#and a matrix that provides the number of times each team has played each other.
MostellerLARC <- function(strengths,wins,magnificationfactor=1) {
  PIPI <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      PIPI <- PIPI*pnorm(strengths[i]-strengths[j])^wins[i,j]*magnificationfactor
    }
  }
  return(PIPI)
}

#an example of the function
MostellerLARC(NBAdf$Strength,NBAdf$WinsVersus)

# LARC.Optim: this function simplifies the optimization of LARC. It only needs a dataframe
#but can take up to two more: details which is either TRUE or FALSE dictates whether or not
#the details oof the optimization is shown (FALSE is the default) and func which takes the
#function for the strengths to be optimized over (the default is BradleyTerryLARC)
LARC.Optim <- function(df,details=FALSE,func=BradleyTerryLARC){
  library(optimx)
  if(details==TRUE){
    optimx(df$Strength,func,wins=df$WinsVersus,versus=df$Versus,method="L-BFGS-B"
           ,lower=0,upper=Inf,control=list(trace=6,fnscale=-1,maxit=99999,REPORT=1))
  } else {
    optimx(df$Strength,func,wins=df$WinsVersus,versus=df$Versus,method="L-BFGS-B"
           ,lower=0,upper=Inf,control=list(fnscale=-1,maxit=99999,REPORT=1))
  }
}

#an example of this function optimizing the NBA data's strengths and showing the details
LARC.Optim(NBAdf,TRUE)
LARC.Optim(NHLdf)
#LARC.Optim(NFLdf,TRUE)

# LARC.Rank: this function optimizes the LARC strengths and then creates a new dataframe
#that includes those new strengths and ranks the teams according to them. It only needs a
#dataframe, but can also take a function to optimize over and the number of decimals that
#should be reported for the updated strengths(the default is BradleyTerryLARC)
LARC.Rank <- function(df, func=BradleyTerryLARC, increment = 0.001, 
                      iterations = Inf, dgt=3, magnificationfactor=1) {
  options(digits=dgt)
  tt <- nrow(df)
  
  if (is.null(df$WinsTotal)) {
    for (i in 1:tt) {
      df$WinsTotal[i] <- sum(df$WinsVersus[i,])
    }
  }
  
  magnificationfactor <- find.mf(df,magnificationfactor,func)
  optimized <- LARC.Optim(df, func, increment, iterations, magnificationfactor)
  df$UpdatedStrength <- round(optimized$UpdatedStrengths,dgt)
  
  TempOrder <- df[order(-df$UpdatedStrength,-df$WinsTotal),]
  Ranked <- data.frame(1:tt,TempOrder$Team,TempOrder$UpdatedStrength,TempOrder$WinsTotal)
  names(Ranked) <- c("Rank","Team","Strength","WinsTotal")
  
  Ranking <- data.frame(Ranked$Rank,as.character(Ranked$Team),Ranked$Strength,Ranked$WinsTotal)
  names(Ranking) <- c("Rank","Team","Strength","WinsTotal")
  return(Ranking)
}

#an example of this function creating a Ranking dataframe named NBARanking
NBARanking <- LARC.Rank(NBAdf)
NHLRanking <- LARC.Rank(NHLdf)
#NFLRanking <- LARC.Rank(NFLdf)

#The following two functions simply generate the probability as dictated by either the
# Bradley-Terry or Mosteller Models

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

#MWP (or Mosteller Win Probability) functions exactly the same as BTWP.
MWP <- function(team1strength,team2strength, statement = TRUE, team1 = NULL, team2 = NULL) {
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

find.mf <- function(df, mf = 1, func = BradleyTerryLARC) {
  while (func(df$Strength,df$WinsVersus,mf) == 0) {
    mf = mf + 1
  }
  while (func(df$Strength,df$WinsVersus,mf) == Inf) {
    mf = mf - 1
  }
  return(mf)
}

LARC.Posterior <- function(df, func = BradleyTerryLARC, mf = 1) {
  posterior <- func(df$Strength,df$WinsVersus,mf)
  while (posterior == 0) {
    mf = mf + 1
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  while (posterior == Inf) {
    mf = mf - 1
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  return(posterior)
}
