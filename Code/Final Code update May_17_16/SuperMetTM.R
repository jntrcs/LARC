###A file to run a Metropolis Hastings algorithm on the supercomputer

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

load("Stripped2016FootballData.RData")
rnormSD=.1
dat<-MetHast(logTMDensity, winsMatrix = stripped2016data[[15]][[1]]$WinsVersus, rnormSD=rnormSD, useTimer = T, time=2260)
print("num rejects: " )
      print(dat[[2]])
      print("Dimensions")
print(rnormSD)
print(dim(dat[[1]]))

finalTM<-handleBurnIn(dat[[1]], 3000)
rm(dat)
finalTM<-useEvery(finalTM, 300)
save(finalTM, file="MetHastResultsTMActual.RData")
