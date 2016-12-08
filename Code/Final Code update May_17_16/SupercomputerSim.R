#The file that will actually be submitted to the supercomputer to run

library(Rcpp)
load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

library(parallel)
numCores<-1
clust <- makeCluster(numCores)
neededFunc<- c("dataconfigure", "LARC.Rank.Football", "BTDensity", "TMDensity",
               "ThurstoneMostellerLARC",   "BradleyTerryLARC", "LARC.Rank", "LARC.Optim",
               "find.mf","attachMostRecentStrengths", "TMDensity", "BTDensity",
               "pickTMStrength", "pickBTStrength", "simHomeWin", "generateTeams","generateSchedule", 
               "generateConference","generateNonConference", "generateTeamSchedule", "generateSeasonResults",
        "predictionPercentage", "simulate1", "findMSE", "normalizeSample")
clusterExport(clust, neededFunc)

parLapply(clust, 1:1, fun = function(i){Rcpp::sourceCpp("cppFiles.cpp")
  useBT <- i%%2==0
  save(simulate1(useBT), file=paste0("~/season",i, ".rdata"))})
#save(output, file="Simulation.RData")
