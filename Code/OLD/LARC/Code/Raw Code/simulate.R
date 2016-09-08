# This file contains the first simulation senario that Dr. Ross Larsen wrote
#on the white board and two functions written for the purpose of optimization.

#the first chunk of code is just creating a partial data frame of the scenario
# mentioned above.
sim1 <- data.frame(c("A","B","C","D","E"),c(1.3,0.05,0.5,0.75,1))
vs <- matrix(c(0,1,2,0,3,
               1,0,5,2,1,
               2,5,0,1,2,
               0,2,1,0,3,
               3,1,2,3,0),5,5)
names(sim1) <- c("Team","Strength")
sim1$Versus <- vs
sim1$Team <- as.character(sim1$Team)

#The simulate function simulates the season of partial simulated data frame
# using as a second arguement either BTWP or MWP for deciding the wins and
# loses of the teams in teh simulation. This returns a list a matrix of the
# wins/loses (WinsVersus) and a vector of the teams total wins (WinsTotal)
# both of which can be added to the original data frame.
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
  WinsTotal <- rowSums(WinsVersus)
  
  return(list(WinsVersus=WinsVersus,WinsTotal=WinsTotal))
}

#runs two examples of simulate, one using Bradley-Terry and the other using Mosteller
go <- proc.time()
sim1$WinsVersus <- simulate(sim1)$WinsVersus
LARC.Optim(sim1)
LARC.Rank(sim1)

sim1$WinsVersus <- simulate(sim1,MWP)$WinsVersus
LARC.Optim(sim1,MostellerLARC)
LARC.Rank(sim1,MostellerLARC)
proc.time() - go

#The simulations function uses the above simulate function and then runs as many
# simulations as you tell it to with the second arguement (which is simulations and
# is set to 1000 as a default). It only requires a data frame in the same style as
# required by simulate; however, the third arguemment, type, is also very useful as
# a control to the type of simulation run. It is default to "BT" for Bradley-Terry
# but will also take "M" for Mosteller simulation. This function outputs a list that
# includes four parts: SimulatedData provides a summary of the averaged wins and
# strengths of all the simulations in a small data frame, AllStrengths provides a
# matrix of all the strengths simulated where each row is the strengths simulated for
# each team (this can be used to create a histogram of the strengths), Variance provides
# a vector of the variances for each team's simulated strengths, and Bias provides a
# vector of the biases for each simulated strength average (where bias is calculated by
# dividing the simulated strength by the average of all simulated strengths and then
# subtracting the score for the original strengths, perfect bias would be 0).
# Note: "BT" runs faster than "M"
simulation <- function(df, simulations = 1000, type = "BT", compares = FALSE) {
  N <- nrow(df)
  strengths <- rep(0, N)
  wins <- rep(0, N)
  strong <- matrix(0, N, simulations)
  vars <- rep(0, N)
  prcnt <- vector()
  for (i in 1:simulations) {
    simdf <- df
    if (type == "BT") {
      sim <- simulate(simdf)
      simdf$WinsVersus <- sim$WinsVersus
      simstrength <- LARC.Optim(simdf)
    }
    if (type == "M") {
      sim <- simulate(simdf,MWP)
      simdf$WinsVersus <- sim$WinsVersus
      simstrength <- LARC.Optim(simdf,MostellerLARC)
    }
    if (compares == TRUE) {
      if (i==1) {
        counts <- LARC.Compare(simdf)$Comparison
      } else {
        counts <- counts + LARC.Compare(simdf)$Comparison
      }
    }
    
    simdf$US <- simstrength$UpdatedStrengths
    prp <- table(c(df[order(-df$Strength),]$Team,1,0)==c(simdf[order(-simdf$US),]$Team,0,1))
    prp <- prp-1
    prcnt <- c(prcnt,prop.table(prp)[2])
    
    strong[,i] <- simstrength$UpdatedStrengths
    strengths <- ((i - 1) * strengths + simstrength$UpdatedStrengths)/i
    wins <- ((i - 1) * wins + sim$WinsTotal)/i
  }
  vars <- vector()
  for (i in 1:N) {
    vars[i] <- var(strong[i,])
  }
  bi <- strengths/mean(strengths) - df$Strength/mean(df$Strength)
  newdf <- data.frame(df$Team, strengths, wins)
  names(newdf) <- c("Team","SimulatedStrength","WinsSummary")
  if (compares == TRUE) {
    return(list(SimulatedData=newdf,AllStrengths=strong,Variance=vars,Bias=bi,PercentPerfect=prcnt
                ,Comparison=counts,Proportion=prop.table(counts)))
  } else {
    return(list(SimulatedData=newdf,AllStrengths=strong,Variance=vars,Bias=bi,PercentPerfect=prcnt))
  }
}

#This provides an example of the simulation function and ways to display its
# output using both Bradley-Terry or Mosteller.
go <- proc.time()
simu.BT <- simulation(sim1, 1000, "BT")
simu.BT$SimulatedData
#this loop and the proceeding one create a histogram of strengths for each team
for (i in 1:5) { hist(simu.BT$AllStrengths[i,], main=sim1$Team[i]) }
simu.BT$Variance
simu.BT$Bias
simu.BT$Proportion
simu.BT$Comparison
proc.time() - go
go <- proc.time()
simu.M <- simulation(sim1, 1000, "M")
simu.M$SimulatedData
for (i in 1:5) { hist(simu.M$AllStrengths[i,], main=sim1$Team[i]) }
simu.M$Variance
simu.M$Bias
simu.M$Proportion
simu.M$Comparison
proc.time() - go

ggplot(as.data.frame(simu.BT$Proportion)[1:2,],aes(Var1,Freq,fill=Var1)) + 
  geom_bar(stat = "identity",fill=c("darkgray","mediumpurple1")) + expand_limits(y=1) +
  labs(title="Side by Side Model Comparison",x=
         "1000 simulations of a 20 game scenario were run and the model that more accurately predicted each game was recorded."
       ,y="") +
  theme(axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=14),title=element_text(size=16))

ggplot(as.data.frame(simu.M$Proportion)[1:2,],aes(Var1,Freq,fill=Var1)) + 
  geom_bar(stat = "identity",fill=c("darkgray","mediumpurple1")) + expand_limits(y=1) +
  labs(title="Side by Side Model Comparison",x=
         "1000 simulations of a 20 game scenario were run and the model that more accurately predicted each game was recorded."
       ,y="") +
  theme(axis.title.x=element_text(size=10),
        axis.text.x=element_text(size=14),title=element_text(size=16))
