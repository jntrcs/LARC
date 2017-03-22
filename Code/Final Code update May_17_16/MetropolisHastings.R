##Metropolis Hastings algorithm: needs to be improved, we used mode estimates of strengths instead of this.
load("MasterFunctionFile.RData")

Rcpp::sourceCpp("cppFiles.cpp")




analyzeMHMatrix<-function(dat)
{
  means<-apply(dat, 1, mean)
  upperBound<-apply(dat, 1, function(i){quantile(i, .975)})
  lowerBound<-apply(dat, 1, function(i){quantile(i, .025)})
  a<-data.frame(means, lowerBound, upperBound)
  names(a)<-c("Mean", "LowerBound", "UpperBound")
  return(a)
}

handleBurnIn<-function(matDat, numToRemove)
{
  return(matDat[,(numToRemove+1):ncol(matDat)])
}

useEvery<-function(matDat, n)
{
  matDat[seq(from=1, to=nrow(matDat), by=n),]
}

newMetHast<-function(func, nSamples=10000, winsMatrix, sig=1.8)
{
  winsTotal<-apply(winsMatrix, 1, sum)
  p<-nrow(winsMatrix)
  chain<-matrix(0, nrow=p, ncol=nSamples)
  chain[,1]<-ifelse(identical(logBTDensity, func), 1,0) #Starting values
  mostRecent<-chain[,1]
  mostRecentFunc<-func(mostRecent, winsMatrix, winsTotal)
  acc<-0
  for (j in 2:nSamples)
  {
    for (i in 1:p)
    {
      cand<-rnorm(1, mostRecent[i], sig)
      newStrengths<-mostRecent
      newStrengths[i]<-cand
      if (identical(logBTDensity, func) & cand<0)
      {
        r=-Inf
      }
      else{
        new<-func(newStrengths, winsMatrix, winsTotal)
      r<-new-mostRecentFunc}
      if (r>=log(runif(1)))
      {
        mostRecent[i]<-cand
        mostRecentFunc<-new
        chain[i, j]<- cand
        acc<-acc+1
      }
      else{
        chain[i,j]<-chain[i,j-1]
      }
    }
  }
  return(list(Matrix=chain, Acceptance=acc/(nSamples*p)))
}

a=newMetHast(logBTDensity, nSamples = 1000, stripped2016data[[14]][[1]]$WinsVersus, sig=1.5)
dat<-a[[1]]
plot(dat[3,])
apply(dat,1,mean)
require(coda)
sim<-as.mcmc(dat)
autocorr(sim)
MetHast<-function(func, nSamples=NULL, winsMatrix, rnormSD=.1, useTimer=F, time=NULL) ###warning: using the timer method currently uses 5GB of RAM 
{
  
  if(identical(func, logBTDensity))
    BT<-T
  else if (identical(func, logTMDensity))
    BT=F
  else
    return("ERROR. Did you remember to use a log likelihood density?")
  
  start<-Sys.time()
  if(useTimer)
  {
    nSamples<-4000000
  }
  else
    time<-0
  nTeams<-nrow(winsMatrix)
  winTotals<-apply(winsMatrix, 1, sum)
  #curve(dnorm(x,0,.1), xlim=c(-1,1))
  initial<-rep(ifelse(BT, 1, 0), nTeams)
  dist<-matrix(0, ncol=nTeams, nrow=nSamples)
  dist[1,]<-initial
  rowCounter<-2
  lastDensity<-func(initial,winsMatrix, winTotals)
  rejections<-0
  while(rowCounter<=nSamples & (!useTimer |Sys.time()<start+time))
  {
   
    changeVec<-rnorm(nTeams, 0,rnormSD)
    newStrengths<-dist[rowCounter-1,]+changeVec
    if (BT & any(newStrengths<=0))
      {newDensity<-0
       probab<-0}
    else
      {newDensity<-func(newStrengths, winsMatrix, winTotals)
      probab<-exp(newDensity-lastDensity)}
    if (is.na(probab))
    {
      print(newStrengths)
      print(newDensity)
    }
    if (runif(1)<probab)
    {
      nextDraw<-newStrengths
      lastDensity<-newDensity
    }
    else 
     { 
       rejections<-rejections+1
       nextDraw<-dist[rowCounter-1,]
     }
     dist[rowCounter,]<-nextDraw
     rowCounter<-rowCounter+1
 
  }
  return(list(dist[1:(rowCounter-1),], rejections))
}

week15<-MetHast(logBTDensity, winsMatrix=stripped2016data[[15]][[1]]$WinsVersus,rnormSD=.04,useTimer = T, time=10)
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

employee<-MetHast(logBTDensity, 5000, Employeedf$WinsVersus, .3)
employeem<-handleBurnIn(employee[[1]], 1000)
employeem<-useEvery(employeem, 10)
s1<-employeem[,1]
means<-apply(employee[[1]],2,mean)
means
plot(s1)
hist(s1)
ord<-order(means, decreasing=T)
upperBound<-apply(employeem, 2, FUN=function(i){quantile(i,.975)})[ord]
lowerBound<-apply(employeem, 2, FUN=function(i){quantile(i,.025)})[ord]
cbind(EmployeeRank, Employeedf$Team[ord], means[ord], lowerBound, upperBound)

means<-apply(final,2,mean)
o<-order(means, decreasing=T)
teamRank<-stripped2016data[[15]][[1]]$Team[o]
upperBound<-apply(final, 2, FUN=function(i){quantile(i,.975)})[o]
lowerBound<-apply(final, 2, FUN=function(i){quantile(i,.025)})[o]


meansNew<-apply(finalBT,2,mean)
ord<-order(meansNew, decreasing=T)
teamRankNew<-stripped2016data[[15]][[1]]$Team[ord]
upperBoundN<-apply(finalBT, 2, FUN=function(i){quantile(i,.975)})[ord]
lowerBoundN<-apply(finalBT, 2, FUN=function(i){quantile(i,.025)})[ord]
cbind(stripped2016data[[15]][[2]], teamRankNew, meansNew[o], lowerBoundN, upperBoundN)
hist(final[,3])
stripped2016data[[15]][[5]]

