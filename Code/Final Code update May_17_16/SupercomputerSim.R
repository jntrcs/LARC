#The file that will actually be submitted to the supercomputer to run

library(Rcpp)
load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

library(parallel)
numCores<-10
clust <- makeCluster(numCores)
neededFunc<- c("dataconfigure", "LARC.Rank.Football", "BTDensity", "TMDensity",
               "ThurstoneMostellerLARC",   "BradleyTerryLARC", "LARC.Rank", "LARC.Optim",
               "find.mf","attachMostRecentStrengths", "TMDensity", "BTDensity",
               "pickTMStrength", "pickBTStrength", "simHomeWin", "generateTeams","generateSchedule", 
               "generateConference","generateNonConference", "generateTeamSchedule", "generateSeasonResults")
clusterExport(clust, neededFunc)

output<- parLapply(clust, 1:10, fun = function(i){Rcpp::sourceCpp("cppFiles.cpp")
  useBT <- i%%2==0
  simulate1(useBT)})
save(output, file="Simulation.RData")