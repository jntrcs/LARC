#The file that will actually be submitted to the supercomputer to run
load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

library(parallel)
numCores<-4
clust <- makeCluster(numCores)
neededFunc<- c("dataconfigure", "LARC.Rank.Football", "BTDensity", "TMDensity",
               "ThurstoneMostellerLARC",   "BradleyTerryLARC", "LARC.Rank", "LARC.Optim",
               "find.mf","attachMostRecentStrengths", "TMDensity", "BTDensity",
               "pickTMStrength", "pickBTStrength", "simHomeWin", "generateTeams","generateSchedule", 
               "generateConference","generateNonConference", "generateTeamSchedule", "generateSeasonResults",
               "predictionPercentage", "simulate1", "findMSE", "normalizeSample","getConferenceMeans", 
               "pickBetaStrength","analyzeGameBias")
clusterExport(clust, neededFunc)

parLapply(clust, 1:4, fun = function(i){
  Rcpp::sourceCpp("cppFiles.cpp")
  useBT <- i<=2
  useBeta<-i>3
extremeBT<-i<=1
  filename<-ifelse(useBT, ifelse(extremeBT, "ExtremeBT.RData", "BT.RData"), ifelse(useBeta, "Beta.RData", "TM.RData"))
  load(filename)
  dat<-simulate1(useBT, useBeta, extremeBT,strengths)
  save(dat, file=paste0("~/LARC/LARC/Code/Final\ Code\ update\ May_17_16/Results/season",i, ".rdata"))
  i})


