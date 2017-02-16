#Game Bias Analysis
load("SimulationResults.RData")

gameBiasGraph<-function(BTMatrix, TMMatrix, type)
{
  plot(1, type='n', main = "Win Prediction Probability MSE",
       ylab="Predicted Probability MSE", xlab="Week", ylim=c(0,.1), xlim=c(1,13), xaxt='n')
  legend("topright", legend=c("Bradley-Terry", "Thurstone-Mosteller", "95% Quantiles"), title=paste(type, "True Strengths"),
         col=c("black", "gray", "gray13"), lty = c(1,2,1), lwd=c(4,4,1))
  rect(0,-1,3,1,col = rgb(0.5,0.5,0.5,1/4), border=NA)
    legend("bottomleft", legend="Shade represents non-conference weeks")
  BTmeans<-apply(BTMatrix, 1, mean) 
  lines(BTmeans, col="Black", type='l', lwd=4)
  BTsds<-apply(BTMatrix, 1, sd)
  qBT<-apply(BTMatrix, 1, FUN=function(d){quantile(d, c(.025,.975))})
  lines(qBT[1,], lwd=1, col="gray14")
  lines(qBT[2,],lwd=1, col="gray14")
  qTM<-apply(TMMatrix, 1, FUN=function(d){quantile(d, c(.025,.975))})
  lines(qTM[1,], col="gray", lty=2)
  lines(qTM[2,], col="gray", lty=2)
  
  
  serror<-qnorm(.975)*BTsds/sqrt(ncol(BTMatrix))
  #lines(BTmeans+serror, col="firebrick1", lty=2)
  #lines(BTmeans-serror, col="firebrick1", lty=2)
  TMmeans<-apply(TMMatrix, 1, mean)
  TMsds<-apply(TMMatrix, 1, sd)
  serror<-qnorm(.975)*TMsds/sqrt(ncol(TMMatrix))
  #lines(TMmeans+serror, lty=2, col="steelblue1")
  #lines(TMmeans-serror, lty=2, col="steelblue1")
  lines(TMmeans, col="gray", lwd=4, lty=2, type='l')
  axis(1,at=1:12,labels=2:13)
  
}

suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")
gameBiasMatrix<-list()
for (i in suffix)
{
  gameBiasMatrix[[i]]<-list(BT=list(), TM=list())
  gameBiasMatrix[[i]]$BT<-sapply(weeklyGameBias[[i]], FUN=function(n){n$BT.MSE})
  gameBiasMatrix[[i]]$TM<-sapply(weeklyGameBias[[i]], FUN=function(n){n$TM.MSE})
}
par(bg="gray99")
par(mfrow=c(1,1))
gameBiasGraph(gameBiasMatrix$BradleyTerryGamma$BT, gameBiasMatrix$BradleyTerryGamma$TM, "Bradley-Terry")
gameBiasGraph(gameBiasMatrix$Beta$BT, gameBiasMatrix$Beta$TM, "Beta")
gameBiasGraph(gameBiasMatrix$ThurstoneMostellerNormal$BT, gameBiasMatrix$ThurstoneMostellerNormal$TM, "Thurstone-Mosteller")
gameBiasGraph(gameBiasMatrix$ExtremeBT$BT, gameBiasMatrix$ExtremeBT$TM, "Extreme Bradley Terry")


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
