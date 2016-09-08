# A function for optimization that will quickly find the optimized strengths for 
#either the Bradley-Terry of Mosteller Models.

#LARC.Optim takes up to four arguements but only requires one a data frame to have
# the strengths of it's respective teams to be optimized. The second arguement is
# func and is the most important of the optional arguements as it is set to default
# with the BradleyTerryLARC function but can also take the MostellerLARC function.
# The third and fourth arguements hardly ever need to be touched they are increment
# and iterations and are set to 0.001 and infinity respectively, but they can be
# changed to indicate how many decimals it should optimize to or to limit the 
# iterations processed before giving a result.
LARC.Optim <- function(df, func = BradleyTerryLARC, increment = 0.001, 
                       iterations = Inf, magnificationfactor=1) {
  st <- df$Strength
  wv <- df$WinsVersus
  magnificationfactor <- find.mf(df,magnificationfactor,func)
  comp <- func(st,wv,magnificationfactor)
  
  #tells r to round to return results to the decimal place indicated by increment
  options(digits = min(which( increment*10^(0:20)==floor(increment*10^(0:20)) )) - 1)
  
  #tells the function to begin adjusting the strengths by only one decimal place
  inc <- 0.1
  last <- Inf
  x <- 0
  
  #the while statement tells it to stop optimizing when the iteration cap is reached
  # or when the incrament is too small.
  while ((comp != last & x < iterations) | inc >= increment) {
    last <- comp
    #the for loop adjust each strength either up or down by inc until func is maxed
    for (i in 1:nrow(df)) {
      st[i] <- st[i] + inc
      magnificationfactor <- find.mf(df,magnificationfactor,func)
      new <- func(st,wv,magnificationfactor)
      if (comp > new) {
        st[i] <- st[i] - 2*inc
        magnificationfactor <- find.mf(df,magnificationfactor,func)
        new <- func(st,wv,magnificationfactor)
        if (comp > new) {
          st[i] <- st[i] + inc
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
  return(list(UpdatedStrengths=st,MaximizedPosterior=comp,Iterations=x))
}

#the reamining code runs an example of the code and times it with the NBAdf but
# changing the second line to make a different df -> sample can change what is used.
go <- proc.time()
Sample <- NBAdf
Optimed <- LARC.Optim(Sample)
BradleyTerryLARC(Sample$Strength,Sample$WinsVersus,Sample$Versus)
Optimed$MaximizedPosterior
NewSample <- data.frame(Sample$Team,Optimed$UpdatedStrengths,Sample$WinsTotal)
NewSampleRank <- NewSample[order(-NewSample$'Optimed.UpdatedStrengths'),]
NewSampleRank
proc.time() - go
