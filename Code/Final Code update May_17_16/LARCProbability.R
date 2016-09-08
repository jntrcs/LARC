#This file contains the LARC.Prob function which combs through the data and drops
# each game one at a time and attempts to predict the outcome of that game. It can go
# through the data and do this for every single game and this the best way to do it,
# it is the default but note that it takes a very long time. Taking a random draw of
# games is considerably faster but creates extremely inconsistent results. The 
# function only requires a completed data frame, but also can take three other 
# arguements: random=NULL if set to NULL the function will predict every game otherwise
# this takes a number for how many random games to predict, and type="BT" to dictate 
# between Bradley-Terry and "TM" Thurstone-Mosteller. The function returns a vector of the 
# probabilities that the actual winner of each game should win. 

LARC.Prob <- function(df, random=NULL, type = "BT") {
  N <- nrow(df)
  #Gets the random draws if needed and creates the loop parameters
  if (is.null(random)==FALSE) {
    samp <- sample(1:N^2, random, FALSE)
  } else {
    samp <- 1:N^2
  }
  probs <- vector()
  #loops but only does calculations if any games are played, ignores values of zero
  for (i in samp) {
    if (df$WinsVersus[i] != 0) {
      reps <- df$WinsVersus[i]
      #removes the game
      df$WinsVersus[i] <- df$WinsVersus[i] - 1
      #chooses either "BT" or "M" based on the arguements
      if (type == "BT") {
        US <- LARC.Optim(df)$UpdatedStrengths
        #if that match up happened more than once it replicates it in the data to show that
        for (j in 1:reps) {
          probs <- c(probs,
                     BTWP(US[ifelse(i%%N==0,N,i%%N)],US[ifelse(i%%N==0,i%/%N,i%/%N+1)],FALSE))
        }
      } else {
        US <- LARC.Optim(df, ThurstoneMostellerLARC)$UpdatedStrengths
        for (j in 1:reps) {
          probs <- c(probs,
                     MWP(US[ifelse(i%%N==0,N,i%%N)],US[ifelse(i%%N==0,i%/%N,i%/%N+1)],FALSE))
        }
      }
      #reinputs the removed game for the next loop
      df$WinsVersus[i] <- df$WinsVersus[i] + 1
    }
  }
  return(probs)
}

#The following is a running of this code using the first simulation. The results can
# be used quite nicely to create a histogram. It should be evaluated that a histogram
# with one peak around 1.0 and a very small tail eanding before or near 0.5 is best 
# results with two peaks at 1.0 and 0.5 could also be considered very good.
go <- proc.time()
sampled <- sim1
probabilitiesBT <- LARC.Prob(sampled)
head(probabilitiesBT)
hist(probabilitiesBT)
clock <- proc.time() - go
clock
