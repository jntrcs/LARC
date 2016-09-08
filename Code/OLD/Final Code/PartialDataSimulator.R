#This function simulates the season of a partial data frame like those created by the sim.gen 
# function. Taking that data frame as the first arguement and as a second arguement either BTWP
# or TMWP for deciding the wins and loses of the teams for the simulation. This returns a list 
# including a WinsVersus matrix and a WinsTotal vector both of which can be easily added to the 
# original data frame.
simulate <- function(df, prob.f = BTWP) {
  WinsVersus <- matrix(0,nrow(df),nrow(df))
  WinsTotal <- 0
  #the two loops and if statement make sure no games are repeated from the 
  # Versus matrix
  for (i in 1:length(df$Strength)) {
    for (j in (i+1):length(df$Strength)) {
      if (j < length(df$Strength)+1) {
        #generates the probability of the first team winning
        pr <- prob.f(df$Strength[i],df$Strength[j],FALSE)
        #randomly decides who wins each game using the generated probability
        vv <- sample(c(df$Team[i],df$Team[j]),df$Versus[i,j],TRUE,prob=c(pr,1-pr))
        # adds these wins to the new WinsVersus matrix
        WinsVersus[i,j] <- table(vv)[df$Team[i]]
        WinsVersus[j,i] <- table(vv)[df$Team[j]]
      }
    }
  }
  WinsVersus[is.na(WinsVersus)] <- 0
  #creates the WinsTotal vector
  WinsTotal <- rowSums(WinsVersus)
  
  return(list(WinsVersus=WinsVersus,WinsTotal=WinsTotal))
}

#the following is an example of using this function to amend and add to partial data.
sim1 <- sim.gen(5, 20)
parts1 <- simulate(sim1)
sim1$WinsTotal <- parts1$WinsTotal
sim1$WinsVersus <- parts1$WinsVersus
sim1
