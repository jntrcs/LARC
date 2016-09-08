# This file contains the LARC.Prob function which combs through the data and drops
#each game one at a time and attempts to predict the outcome of that game. It can go
#through the data and do this for every single game and this the best way to do it,
#it is the default but note that it takes a very long time. Taking a random draw of
#games is considerably faster but creates extremely inconsistent results. The 
#function only requires a completed data frame, but also can take three other 
#arguements: random=NULL if set to NULL the function will predict every game otherwise
#this takes a number for how many random games to predict, and type="BT" to dictate 
#between Bradley-Terry and "M" Mosteller. The function returns a vector of the 
#probabilities that the actual winner of each game should win. 

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
        US <- LARC.Optim(df, MostellerLARC)$UpdatedStrengths
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
#be used quite nicely to create a histogram. It should be evaluated that a histogram
#with one peak around 1.0 and a very small tail eanding before or near 0.5 is best 
#results with two peaks at 1.0 and 0.5 could also be considered very good.
go <- proc.time()
sampled <- sim1
probabilitiesBT <- LARC.Prob(sampled)
head(probabilitiesBT)
hist(probabilitiesBT)
proc.time() - go

go <- proc.time()
probabilitiesM <- LARC.Prob(sampled,type = "M")
head(probabilitiesM)
hist(probabilitiesM)
proc.time() - go

LARC.Compare <- function(df) {
  BTP <- c(LARC.Prob(df, type = "BT"),0,1,0.5)
  MP <- c(LARC.Prob(df, type = "M"),1,0,0.5)
  cmprsn <- table(ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")))-1
  prcntgs <- prop.table(cmprsn)
  ss <- sum(df$WinsVersus)
  BTP <- BTP[1:ss]
  MP <- MP[1:ss]
  Team <- vector()
  Versus <- vector()
  N <- nrow(df)
  for (i in 1:N^2) {
    if (df$WinsVersus[i] != 0) {
      reps <- df$WinsVersus[i]
      for (j in 1:reps) {
        Team <- c(Team,df$Team[ifelse(i%%N==0,N,i%%N)])
        Versus <- c(Versus,df$Team[ifelse(i%%N==0,i%/%N,i%/%N+1)])
      }
    }
  }
  VisualComp <- data.frame(Winner=Team,Versus,BradleyTerryProbabilities=BTP,MostellerProbabilities=MP,
                           Model=ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")),
                           Difference=MP-BTP)
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp)
  class(lst) = "prthdn"
  return(lst)
}

print.prthdn = function(x, ...){
  x = x["Proportion"]
  NextMethod()
}

sim.t1$WinsVersus <- simulate(sim.t1,BTWP)$WinsVersus
LARC.Rank(sim.t1)
(funM <- LARC.Compare(sim.t1))
sim.t1$WinsVersus <- simulate(sim.t1,MWP)$WinsVersus
LARC.Rank(sim.t1,func=MostellerLARC)
(funBT <- LARC.Compare(sim.t1))
simulation(sim.t1, type = "BT")$SimulatedData
simulation(sim.t1, type = "M")$SimulatedData
LARC.Rank(false.t1)
LARC.Rank(false.t1,func=MostellerLARC)
(fun <- LARC.Compare(false.t1))
#View(fun$DataTable)
fun$Comparison

table(ifelse(probabilitiesBT>probabilitiesM,"Bradley-Terry",
             ifelse(probabilitiesBT<probabilitiesM,"Mosteller","Tie")))

# Additional Note: It might be cool to create a simulation that uses the updated strengths
#to determine future wins...?

#go.all <- proc.time()
#go <- proc.time()
#(NBAComp <- LARC.Compare(NBAdf))
#proc.time() - go
#go <- proc.time()
#(NHLComp <- LARC.Compare(NHLdf))
#proc.time() - go
#go <- proc.time()
#(NFLComp <- LARC.Compare(NFLdf))
#proc.time() - go
#proc.time() - go.all
