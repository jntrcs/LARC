###SuperMetHast

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

load("Stripped2016FootballData.RData")

dat<-MetHast(logTMDensity, winsMatrix = stripped2016data[[15]][[1]]$WinsVersus, rnormSD=.06, useTimer = T, time=2260)
print("num rejects: " )
      print(dat[[2]])
      print("Dimensions")
print(dim(dat[[1]]))

finalTM<-handleBurnIn(dat[[1]], 3000)
rm(dat)
finalTM<-useEvery(finalTM, 200)
save(finalTM, file="MetHastResultsTMActual.RData")
