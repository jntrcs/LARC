##Metropolis Hastings.....


MetHast<-function(func, nSamples, winsMatrix)
{
  nTeams<-nrow(winsMatrix)
  winTotals<-apply(winsMatrix, 1, sum)
  #curve(dnorm(x,0,.1), xlim=c(-1,1))
  initial<-rep(1, nTeams)
  dist<-matrix(0, ncol=nTeams, nrow=nSamples)
  dist[1,]<-initial
  rowCounter<-2
  lastDensity<-func(initial,winsMatrix, winTotals)
  rejections<-0
  while(rowCounter<=nSamples)
  {
    if(rejections>100000)
      return("Reached 1000000 rejections")
    changeVec<-rnorm(nTeams, 0,.1)
    newStrengths<-dist[rowCounter-1,]+changeVec
    newStrengths[newStrengths<=0]<-0.05
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
    {
      rejections<-rejections+1
      if (rejections>999990)
      {  
        print(probab)
      }
    }
  }
  return(list(dist, rejections))
}

week15<-MetHast(logBTDensity, 10000, winsMatrix)
s1<-week15[[1]][,3]
plot(s1)
means<-apply(week15[[1]],2,mean)
o<-order(means, decreasing=T)
teamRank<-stripped2016data[[15]][[1]]$Team[o]
cbind(stripped2016data[[15]][[2]], teamRank, means[o])
