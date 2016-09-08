#The simulations function uses the simulate function and a partial data frame that can be created
# with the sim.gen function then runs as many simulations of that data as you tell it to with the
# second arguement (which is simulations and is set to 1000 as a default). The first arguement is a
# partial data frame. The third arguemment, type, can either be "BT" or "TM" to indicate the model
# to use to determine the outcomes of each game based on the strengths provided by the partial data
# frame. The final argument is compares and can be either TRUE or FALSE and if it is TRUE the
# function also utilizes the LARC.compare function to see which model would better predict each
# game simulated in each simulation.
# This function outputs a list that includes four parts:
#SimulatedData-provides a summary of the averaged wins and strengths of all the simulations in a
#         small data frame.
#AllStrengths-provides a matrix of all the strengths simulated where each row is the strengths
#         simulated for each team (this can be used to create a histogram of the strengths).
#Variance-provides a vector of the variances for each team's simulated strengths
#Bias-provides a vector of the biases for each simulated strength averages (where bias is calculated
#         by dividing the assigned simulated strength by the average of all simulated strengths and
#         then subtracting the score for the original strengths, perfect bias would be 0).
#PercentPerfect-gives the percent of each loop of the simulation's final ranking that matched the
#         input strengths.
#Comparison-Only returned if compares=TRUE, where this returns a table of how many times each model
#         better predicted the actual outcome of a matchup.
#Proportion-Only returned if compares=TRUE, where this returns a table of the percentage that each
#         model better predicted the actual outcome of a matchup.
simulation <- function(df, simulations = 1000, type = "BT", compares = FALSE) {
  #these first few lines create the empty variables that will be later filled in the loops
  N <- nrow(df)
  strengths <- rep(0, N)
  wins <- rep(0, N)
  strong <- matrix(0, N, simulations)
  vars <- rep(0, N)
  prcnt <- vector()
  #this loop contains the simulation
  for (i in 1:simulations) {
    simdf <- df
    #if statement dictates which model will be used to decide game outcomes then uses simulate to
    # simulate the games outcomes.
    if (type == "BT") {
      sim <- simulate(simdf)
      simdf$WinsVersus <- sim$WinsVersus
      simstrength <- LARC.Optim(simdf)
    }
    if (type == "TM") {
      sim <- simulate(simdf,MWP)
      simdf$WinsVersus <- sim$WinsVersus
      simstrength <- LARC.Optim(simdf,MostellerLARC)
    }
    #if compares is TRUE checks which model would better predict the correct outcome
    if (compares == TRUE) {
      if (i==1) {
        counts <- LARC.Compare(simdf)$Comparison
      } else {
        counts <- counts + LARC.Compare(simdf)$Comparison
      }
    }
    #summarizes the strengths calculated as a result of the simulationof the teams so they can be
    # graphed and evaulated and then calculates the percent perfect value for the round
    simdf$US <- simstrength$UpdatedStrengths
    prp <- table(c(df[order(-df$Strength),]$Team,1,0)==c(simdf[order(-simdf$US),]$Team,0,1))
    prp <- prp-1
    prcnt <- c(prcnt,prop.table(prp)[2])
    #calculates the average strength and wins across the simulation
    strong[,i] <- simstrength$UpdatedStrengths
    strengths <- ((i - 1) * strengths + simstrength$UpdatedStrengths)/i
    wins <- ((i - 1) * wins + sim$WinsTotal)/i
  }
  #calculates the variances and biases of the strengths for each team
  vars <- vector()
  for (i in 1:N) {
    vars[i] <- var(strong[i,])
  }
  bi <- strengths/mean(strengths) - df$Strength/mean(df$Strength)
  #organizes the simulated Data summmary to be returned
  newdf <- data.frame(df$Team, strengths, wins)
  names(newdf) <- c("Team","SimulatedStrength","WinsSummary")
  #this final if statement dictates which list to return based on wether or not compares was TRUE
  if (compares == TRUE) {
    return(list(SimulatedData=newdf,AllStrengths=strong,Variance=vars,Bias=bi,PercentPerfect=prcnt
                ,Comparison=counts,Proportion=prop.table(counts)))
  } else {
    return(list(SimulatedData=newdf,AllStrengths=strong,Variance=vars,Bias=bi,PercentPerfect=prcnt))
  }
}

#This provides an example of the simulation function without compares for Bradley-Terry
go <- proc.time()
simu.BT <- simulation(sim1, 5, "BT")
simu.BT$SimulatedData
#this loop and the proceeding one create a histogram of strengths for each team
for (i in 1:5) { hist(simu.BT$AllStrengths[i,], main=sim1$Team[i]) }
simu.BT$Variance
simu.BT$Bias
simu.BT$PercentPerfect
simu.BT$Comparison
clock <- proc.time() - go
clock

#This provides an example of the simulation function with compares for Thurstone-Mosteller
go <- proc.time()
simu.TM <- simulation(sim1, 5, "TM", TRUE)
simu.TM$SimulatedData
for (i in 1:5) { hist(simu.TM$AllStrengths[i,], main=sim1$Team[i]) }
simu.TM$Variance
simu.TM$Bias
simu.TM$PercentPerfect
simu.TM$Proportion
simu.TM$Comparison
clock <- proc.time() - go
clock
