# LARC.Optim is the function which computes the strengths esimates.
#
# It returns the data frame described in LARC.Rank WITH THE STRENGTHS HAVING THEIR FINAL ESTIMATES
#
# Input Parameters defined below
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
# adj -- Purpose TBD
#
# LARC.Optim invokes the following functions
#     find.mf
#     func
LARC.Optim <- function(df, func = BradleyTerryLARC, increment = 0.001,
                       iterations = Inf, magnificationfactor=1, adj=1) {
  st <- df$Strength
  wv <- df$WinsVersus
  
  mf <- find.mf(df,magnificationfactor,func,adj)
  comp <- func(df$Strength,df$WinsVersus,mf)
  #LARC.Posterior(df, func, mf = magnificationfactor, adj = adj)
  #tells function to round results to the decimal place indicated by increment
  options(digits = min(which( increment*10^(0:20)==floor(increment*10^(0:20)) )) - 1)
  inc <- 0.1
  last <- Inf
  x <- 0
  #the while statement tells it to stop optimizing when the iteration cap is reached
  # or when the incrament is too small.
  while ((comp != last & x < iterations) | inc >= increment) {
    mf <- find.mf(df,mf,func,adj)
    comp <- func(df$Strength,df$WinsVersus,mf)
    last <- comp
    #the for loop adjusts each strength either up or down by inc until func is maxed
    for (i in 1:nrow(df)) {
      df$Strength[i] <- df$Strength[i] + inc
      new <- func(df$Strength,df$WinsVersus,mf)# <- LARC.Posterior(df, func, mf = magnificationfactor,adj=adj)
      if (comp > new) {
        df$Strength[i] <- df$Strength[i] - 2*inc
        new <- func(df$Strength,df$WinsVersus,mf)# <- LARC.Posterior(df, func, mf = magnificationfactor,adj=adj)
        if (comp > new) {
          df$Strength[i] <- df$Strength[i] + inc
        } else {
          comp <- new
        }
      } else {
        comp <- new
      }
    }
    #if func returned the same value it changes to a smaller inc
    if (comp == last) {
      inc <- inc/10
    }
    x <- x + 1
  }
  return(list(UpdatedStrengths=df$Strength,MaximizedPosterior=comp,Iterations=x))
}

#The following it a timed example of the function using an NBA data frame.
# go <- proc.time()
# NBAsimdf$Strength <- LARC.Optim(NBAdf)$UpdatedStrengths
# clock <- proc.time() - go
# clock
