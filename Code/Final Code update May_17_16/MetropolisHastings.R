##Metropolis Hastings.....


handleBurnIn<-function(matDat, numToRemove)
{
  return(matDat[numToRemove:nrow(matDat),])
}

useEvery<-function(matDat, n)
{
  matDat[seq(from=1, to=nrow(matDat), by=n),]
}

MetHast<-function(func, nSamples=NULL, winsMatrix, rnormSD=.1, useTimer=F, time=NULL) ###warning: using the timer method currently uses 5GB of RAM :/
{
  
  if(identical(func, logBTDensity))
    BT<-T
  else if (identical(func, logTMDensity))
    BT=F
  else
    return("ERROR. Did you remember to use a log likelihood density?")
  
  if(useTimer)
  {
    start<-Sys.time()
    nSamples<-4000000
  }
  nTeams<-nrow(winsMatrix)
  winTotals<-apply(winsMatrix, 1, sum)
  #curve(dnorm(x,0,.1), xlim=c(-1,1))
  initial<-rep(ifelse(BT, 1, 0), nTeams)
  dist<-matrix(0, ncol=nTeams, nrow=nSamples)
  dist[1,]<-initial
  rowCounter<-2
  lastDensity<-func(initial,winsMatrix, winTotals)
  rejections<-0
  while(rowCounter<=nSamples &Sys.time()<start+time)
  {
   
    changeVec<-rnorm(nTeams, 0,rnormSD)
    newStrengths<-dist[rowCounter-1,]+changeVec
    if (BT)
      newStrengths[newStrengths<0]<-0
    newDensity<-func(newStrengths, winsMatrix, winTotals)
    probab<-exp(newDensity-lastDensity)
    if (is.na(probab))
    {
      print(newStrengths)
      print(newDensity)
    }
    if (runif(1)<probab)
    {
      dist[rowCounter,]<-newStrengths
      rowCounter<-rowCounter+1
      lastDensity<-newDensity
    }
    else 
      rejections<-rejections+1
   
  }
  return(list(dist[1:rowCounter,], rejections))
}

week15<-MetHast(logTMDensity, winsMatrix=stripped2016data[[15]][[1]]$WinsVersus,rnormSD=.05,useTimer = T, time=10)
week15[[2]]
dim(week15[[1]])
dist<-handleBurnIn(week15[[1]], 500)
dist<-useEvery(dist, 20)
s1<-dist[,22]
plot(s1)
hist(s1)
quantile(s1, c(.025,.975))
means<-apply(dist,2,mean)
o<-order(means, decreasing=T)
teamRank<-stripped2016data[[15]][[1]]$Team[o]
cbind(stripped2016data[[15]][[3]], teamRank, means[o])

employee<-MetHast(logTMDensity, 10000, Employeedf$WinsVersus, .3)
employeem<-handleBurnIn(employee[[1]], 1000)
s1<-employeem[,1]
means<-apply(employee[[1]],2,mean)
means
plot(s1)
hist(s1)
ord<-order(means, decreasing=T)
cbind(EmployeeRank, Employeedf$Team[ord], means[ord])


