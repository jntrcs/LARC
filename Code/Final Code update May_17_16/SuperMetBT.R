###SuperMetHast

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

load("Stripped2016FootballData.RData")
rnormSD=.05
dat<-MetHast(logBTDensity, winsMatrix = stripped2016data[[15]][[1]]$WinsVersus, rnormSD=rnormSD, useTimer = T, time=2260)
print("num rejects: " )
      print(dat[[2]])
      print("Dimensions")
      print(rnormSD)
print(dim(dat[[1]]))

finalBT<-handleBurnIn(dat[[1]], 3000)
rm(dat)
finalBT<-useEvery(finalBT, 300)
save(finalBT, file="MetHastResultsActualBT.RData")
