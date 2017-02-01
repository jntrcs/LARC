###SuperMetHast

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")

load("Stripped2016FootballData.RData")

dat<-MetHast(logTMDensity, winsMatrix = stripped2016data[[15]][[1]]$WinsVersus, rnormSD=.05, useTimer = T, time=20)
print("num rejects: " )
      print(dat[[2]])
      print("Rejections")
print(dim(dat[[1]]))

final<-handleBurnIn(dat[[1]], 2000)
rm(dat)
final<-useEvery(final, 200)
save(final, file="MetHastResults.RData")
