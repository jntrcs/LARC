#Game Bias Analysis
load("SimulationResults.RData")

gameBiasGraph<-function(matrices, type)
{
  plot(1, type='n', main = "Win Prediction Probability MSE",
       ylab="Predicted Probability MSE", xlab="Week", ylim=c(0,.1), xlim=c(1,13), xaxt='n')
  legend("topright", legend=c("Bradley-Terry Mode", "Thurstone-Mosteller Mode", "BT Mean", "TM Mean"), title=paste(type, "True Strengths"),
         col=c("black", "firebrick", "Gray", "Red"), lty = c(1,1,2,2), lwd=4)
  BTmeans<-apply(matrices$BT, 1, mean) 
  lines(BTmeans, col="Black", type='l', lwd=4)
  lines(apply(matrices$BTMean, 1, mean), col="Gray", type='l', lwd=4, lty=2)
  BTsds<-apply(matrices$BT, 1, sd)
  qBT<-apply(matrices$BT, 1, FUN=function(d){quantile(d, c(.025,.975))})
  #lines(qBT[1,], lwd=1, col="gray14")
  #lines(qBT[2,],lwd=1, col="gray14")
  qTM<-apply(matrices$TM, 1, FUN=function(d){quantile(d, c(.025,.975))})
  #lines(qTM[1,], col="gray", lty=2)
  #lines(qTM[2,], col="gray", lty=2)
  
  
  serror<-qnorm(.975)*BTsds/sqrt(ncol(matrices$BT))
  #lines(BTmeans+serror, col="firebrick1", lty=2)
  #lines(BTmeans-serror, col="firebrick1", lty=2)
  TMmeans<-apply(matrices$TM, 1, mean)
  TMsds<-apply(matrices$TM, 1, sd)
  serror<-qnorm(.975)*TMsds/sqrt(ncol(matrices$TM))
  #lines(TMmeans+serror, lty=2, col="steelblue1")
  #lines(TMmeans-serror, lty=2, col="steelblue1")
  lines(TMmeans, col="firebrick", lwd=4, lty=1, type='l')
  lines(apply(matrices$TMMean, 1, mean), col="Red", type='l', lwd=4, lty=2)
  
  axis(1,at=1:12,labels=2:13)
  
}

suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")
gameBiasMatrix<-list()
for (i in suffix)
{
  gameBiasMatrix[[i]]<-list(BT=list(), TM=list())
  gameBiasMatrix[[i]]$BT<-sapply(weeklyGameBias[[i]], FUN=function(n){n$BT.MSE})
  gameBiasMatrix[[i]]$TM<-sapply(weeklyGameBias[[i]], FUN=function(n){n$TM.MSE})
  gameBiasMatrix[[i]]$BTMean<-sapply(weeklyGameBias[[i]], FUN=function(n){n$BTMean.MSE})
  gameBiasMatrix[[i]]$TMMean<-sapply(weeklyGameBias[[i]], FUN=function(n){n$TMMean.MSE})
}
par(bg="gray99")
par(mfrow=c(1,1))
gameBiasGraph(gameBiasMatrix$BradleyTerryGamma, "Bradley-Terry")
gameBiasGraph(gameBiasMatrix$Beta, "Beta")
gameBiasGraph(gameBiasMatrix$ThurstoneMostellerNormal, "Thurstone-Mosteller")
gameBiasGraph(gameBiasMatrix$ExtremeBT, "Extreme Bradley Terry")


suffix<-c("BradleyTerryGamma", "ThurstoneMostellerNormal","Beta", "ExtremeBT")
par(mfrow=c(4,1))
for (i in suffix)
{
  hist(disparity[[i]], main=paste("Team Disparity Score for", i), xlim=c(.1,.35), xlab="Disparity by Season")
  abline(v=mean(disparity[[i]]), col="black", lwd=3)
  #abline(v=mean(disparity[[i]])+c(-1,1)*qnorm(.975)*sd(disparity[[i]]/sqrt(length(disparity[[i]]))), col="Blue", lty=2)
  print(var(disparity[[i]]))
  
}
##Need to make the scales the same