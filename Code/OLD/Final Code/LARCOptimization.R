#A function for optimization that will quickly find the optimized strengths for 
# either the Bradley-Terry of Mosteller Models.

#LARC.Optim takes up to four arguements but only requires one a data frame to have
# the strengths of it's respective teams to be optimized. The second arguement is
# func and is the most important of the optional arguements as it is set to default
# with the BradleyTerryLARC function but can also take the MostellerLARC function.
# The third and fourth arguements hardly ever need to be touched they are increment
# and iterations and are set to 0.001 and infinity respectively, but they can be
# changed to indicate how many decimals it should optimize to or to limit the 
# iterations processed before giving a result. The magnification factor should always
# be fine at one but you may need to make adj smaller so it can change more accurately.
LARC.Optim <- function(df, func = BradleyTerryLARC, increment = 0.001,
                       iterations = Inf, magnificationfactor=1, adj=1) {
  st <- df$Strength
  wv <- df$WinsVersus
  
  mf <- find.mf(df,magnificationfactor,func,adj)
  comp <- func(df$Strength,df$WinsVersus,mf)#LARC.Posterior(df, func, mf = magnificationfactor, adj = adj)
  #tells r to round to return results to the decimal place indicated by increment
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
go <- proc.time()
NBAsimdf$Strength <- LARC.Optim(NBAdf)$UpdatedStrengths
clock <- proc.time() - go
clock
