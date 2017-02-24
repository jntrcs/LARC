##Sample Simulation

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")



lapply(1:8, FUN = function(i){
  useBT <- i<=4
  useBeta<-i>6
  extremeBT<-i<=2
  dat<-simulate1(useBT, useBeta, extremeBT)
  save(dat, file=paste0("season",i, ".rdata"))
  i})
