# Simulation 1

sim1 <- data.frame(c("A","B","C","D","E"),c(1.3,0.05,0.5,0.75,1))
vs <- matrix(c(0,1,2,0,3,
               1,0,5,2,1,
               2,5,0,1,2,
               0,2,1,0,3,
               3,1,2,3,0),5,5)
names(sim1) <- c("Team","Strength")
sim1$Versus <- vs
sim1$Team <- as.character(sim1$Team)

simulate <- function(df, prob.f = BTWP) {
  WinsVersus <- matrix(0,nrow(df),nrow(df))
  WinsTotal <- 0
  for (i in 1:length(df$Strength)) {
    for (j in (i+1):length(df$Strength)) {
      if (j < length(df$Strength)+1) {
        pr <- prob.f(df$Strength[i],df$Strength[j],FALSE)
        vv <- sample(c(df$Team[i],df$Team[j]),df$Versus[i,j],TRUE,prob=c(pr,1-pr))
        WinsVersus[i,j] <- table(vv)[df$Team[i]]
        WinsVersus[j,i] <- table(vv)[df$Team[j]]
      }
    }
  }
  WinsVersus[is.na(WinsVersus)] <- 0
  WinsTotal <- rowSums(WinsVersus)
  
  return(list(WinsVersus=WinsVersus,WinsTotal=WinsTotal))
}

go <- proc.time()
sim1$WinsVersus <- simulate(sim1)$WinsVersus
LARC.Optim(sim1)
LARC.Rank(sim1)

sim1$WinsVersus <- simulate(sim1,MWP)$WinsVersus
LARC.Optim(sim1,MostellerLARC)
LARC.Rank(sim1,MostellerLARC)
proc.time() - go

simulation <- function(df, simulations = 1000, type = "BT") {
  strengths <- rep(0, nrow(df))
  wins <- rep(0, nrow(df))
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
    strengths <- ((i - 1) * strengths + simstrength$UpdatedStrengths)/i
    wins <- ((i - 1) * wins + sim$WinsTotal)/i
  }
  newdf <- data.frame(df$Team, strengths, wins)
  names(newdf) <- c("Team","SimulatedStrength","WinsSummary")
  return(newdf)
}

go <- proc.time()
simulation(sim1, 1000, "BT")
proc.time() - go
go <- proc.time()
simulation(sim1, 1000, "M")
proc.time() - go
