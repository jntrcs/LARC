#The file that was actually submitted to the supercomputer to run
load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

library(parallel)
numCores<-20
clust <- makeCluster(numCores)
neededFunc<- c("dataconfigure", "LARC.Rank.Football", "BTDensity", "TMDensity",
               "ThurstoneMostellerLARC",   "BradleyTerryLARC", "LARC.Rank", "LARC.Optim",
               "find.mf","attachMostRecentStrengths", "TMDensity", "BTDensity",
               "pickTMStrength", "pickBTStrength", "simHomeWin", "generateTeams","generateSchedule", 
               "generateConference","generateNonConference", "generateTeamSchedule", "generateSeasonResults",
               "predictionPercentage", "simulate1", "findMSE", "normalizeSample","getConferenceMeans", 
               "pickBetaStrength","analyzeGameBias", "getWeekMatchups")
clusterExport(clust, neededFunc)

parLapply(clust, 1:400, fun = function(i){
  Rcpp::sourceCpp("cppFiles.cpp")
  useBT <- i<=200
  useBeta<-i>300
extremeBT<-i<=100
  dat<-simulate1(useBT, useBeta, extremeBT)
  save(dat, file=paste0("~/LARC/LARC/Code/Final\ Code\ update\ May_17_16/Results6/season",i, ".rdata"))
  i})


