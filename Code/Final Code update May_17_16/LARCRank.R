# LARC.Rank is the key computation function for computing the strengths of the teams 
# testing 17 Sept 2016
# testing 19 Sept 2016
# This function returns a data frame that has the teams estimated strengths. Also the teams are ranked from highests to lows.
#
# -- the input parameters are defined below
#  
# df -- the data frame containing the data for the strength estimates.  The data from has 5 columns and t rows, where t is the number of teams.
#    ---- Team      -- the names of t teams 
#    ---- Strength  -- the team strengths -- initially these are set equal to 1 -- the code in this function changes the intital strengths to
#                      to 0 if the Mostellor model is being used.
#    ---- WinsTotal -- the number of wins each for each 'team' 
#    ---- Versus    -- a t by t matrix
#                   -- the number of times each team 'plays' each other team 
#    ---- WinVersus -- a t by t matrix
#                   -- the number of times each team beats each other team
#
# func -- which function (Bradley-Terry or Mostellor) is being used
#
# increment -- the precision for which we are optomizing the strengths; default is 0.001, but this can be changed.
#
# iterations -- the maximum number of iterations allowed in the optomizing function; default is infinity, but we could restrict this
# 
# dgt -- purpose TBD
#
# magnificationfactor -- when the posterior calculations become too small, it is necessary to mutiply the posterior calculation by a constant
#                        initially this factor is equal to one, but the code can change this to a large number
#
# adj -- purpose TBD
#
# functions invoked by LARC.Rank
#  LARC.Optim


#A small abstraction so that the last strengths are used when trying to find the new strengths.
#Should speed things considerably
LARC.Rank.Football<-function(df, func=BTDensity, increment = 0.001, 
                             iterations = Inf, dgt=3, magnificationfactor=1, adj=1, sorted=TRUE)
{
  if (identical(func, ThurstoneMostellerLARC)|identical(func, TMDensity) & !is.null(df$TMLast))
    df$Strength<-df$TMLast
  else if ((identical(func, BradleyTerryLARC)|identical(func, BTDensity))& !is.null(df$BTLast))
    df$Strength<-df$BTLast
  
  LARC.Rank(df, func, increment, iterations, dgt, magnificationfactor, adj, football=TRUE, sort=sorted)
}
# 
LARC.Rank <- function(df, func=BTDensity, increment = 0.001, 
                      iterations = Inf, dgt=3, magnificationfactor=1, adj=1, football=FALSE, sorted=TRUE) {
  options(digits=dgt)
  tt <- nrow(df)
  length_strength <- length(df$Strength) # Get the length of strength
  if_mosteller <- identical(func,ThurstoneMostellerLARC)|identical(func, TMDensity) # Check if MostellerLARC function is used
  if (if_mosteller == TRUE && !football){
    for (i in 1:length_strength){
      df$Strength[i] <- 0}# if MostellerLARC then change the initial strength to be Zero
  }
  
  if (is.null(df$WinsTotal)) {
    for (i in 1:tt) {
      df$WinsTotal[i] <- sum(df$WinsVersus[i,])
    }
  }
  
  optimized <- LARC.Optim(df, func, increment, iterations, magnificationfactor,adj=adj)
  df$UpdatedStrength <- round(optimized$UpdatedStrengths,dgt)
  
  if (sorted)
  {
    TempOrder <- df[order(-df$UpdatedStrength,-df$WinsTotal),]
    rank<-1:tt
  }
  
  else
  {
    TempOrder<-df
    #rank<-order(-df$UpdatedStrength)
  }
  Ranked <- data.frame(1:tt,TempOrder$Team,TempOrder$UpdatedStrength,TempOrder$WinsTotal)
  names(Ranked) <- c("Rank","Team","Strength","WinsTotal")
  
  Ranking <- data.frame(Ranked$Rank,as.character(Ranked$Team),Ranked$Strength,Ranked$WinsTotal)
  names(Ranking) <- c("Rank","Team","Strength","WinsTotal")
  return(Ranking)
}

#an example of this function creating a Ranking dataframe named NBARanking
# NBARanking <- LARC.Rank(NBAdf)