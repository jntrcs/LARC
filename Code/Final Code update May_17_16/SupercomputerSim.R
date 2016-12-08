#The file that will actually be submitted to the supercomputer to run

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

library(parallel)
numCores<-10
clust <- makeCluster(numCores)
neededFunc<- c("dataconfigure", "LARC.Rank.Football", "BTDensity", "TMDensity",
               "ThurstoneMostellerLARC",   "BradleyTerryLARC", "LARC.Rank", "LARC.Optim",
               "find.mf","attachMostRecentStrengths", "TMDensity", "BTDensity",
               "pickTMStrength", "pickBTStrength", "simHomeWin", "generateTeams","generateSchedule", 
<<<<<<< HEAD
               "generateConference","generateNonConference", "generateTeamSchedule", "generateSeasonResults", "simulate1")
=======
               "generateConference","generateNonConference", "generateTeamSchedule", "generateSeasonResults",
        "predictionPercentage", "simulate1", "findMSE", "normalizeSample")
>>>>>>> 0ee566e38e8114179ab920add1d396eb81854ae0
clusterExport(clust, neededFunc)

parLapply(clust, 1:10, fun = function(i){
  Rcpp::sourceCpp("cppFiles.cpp")
  useBT <- i%%2==0
  dat<-simulate1(useBT)
  save(dat, file=paste0("~/season",i, ".rdata"))
  i})

