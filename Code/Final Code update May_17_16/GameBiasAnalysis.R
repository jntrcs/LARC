#Game Bias Analysis

gameBiasGraph<-function(BTMatrix, TMMatrix, type)
{
  plot(1, type='n', main = "Win Prediction Probability MSE",
       ylab="Predicted Probability MSE", xlab="Week", ylim=c(0,.1), xlim=c(1,13), xaxt='n')
  legend("topright", legend=c("Bradley-Terry", "Thurstone-Mosteller", "95% CI", "95% Quantiles"), title=paste(type, "True Strengths"),
         col=c("Red", "Blue", "firebrick1", "firebrick1"), lty = c(1,1,2,1))
  BTmeans<-apply(BTMatrix, 1, mean) 
  lines(BTmeans, col="Red", type='l')
  BTsds<-apply(BTMatrix, 1, sd)
  qBT<-apply(BTMatrix, 1, FUN=function(d){quantile(d, c(.025,.975))})
  lines(qBT[1,], col="firebrick1")
  lines(qBT[2,], col="firebrick1")
  qTM<-apply(TMMatrix, 1, FUN=function(d){quantile(d, c(.025,.975))})
  lines(qTM[1,], col="steelblue1")
  lines(qTM[2,], col="steelblue1")
  
  
  serror<-qnorm(.975)*BTsds/sqrt(ncol(BTMatrix))
  lines(BTmeans+serror, col="firebrick1", lty=2)
  lines(BTmeans-serror, col="firebrick1", lty=2)
  TMmeans<-apply(TMMatrix, 1, mean)
  TMsds<-apply(TMMatrix, 1, sd)
  serror<-qnorm(.975)*TMsds/sqrt(ncol(TMMatrix))
  lines(TMmeans+serror, lty=2, col="steelblue1")
  lines(TMmeans-serror, lty=2, col="steelblue1")
  lines(TMmeans, col="Blue", type='l')
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
par(bg="gray90")
par(mfrow=c(1,1))
gameBiasGraph(gameBiasMatrix$BradleyTerryGamma$BT, gameBiasMatrix$BradleyTerryGamma$TM, "Bradley-Terry")
gameBiasGraph(gameBiasMatrix$Beta$BT, gameBiasMatrix$Beta$TM, "Beta")
gameBiasGraph(gameBiasMatrix$ThurstoneMostellerNormal$BT, gameBiasMatrix$ThurstoneMostellerNormal$TM, "Thurstone-Mosteller")
gameBiasGraph(gameBiasMatrix$ExtremeBT$BT, gameBiasMatrix$ExtremeBT$TM, "Extreme Bradley Terry")


suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")
par(mfrow=c(4,1))
for (i in suffix)
{
  hist(disparity[[i]], main=paste("Team Disparity Score for", i), xlim=c(.1,.3), xlab="Disparity by Season")
  abline(v=mean(disparity[[i]]), col="Red")
  abline(v=mean(disparity[[i]])+c(-1,1)*qnorm(.975)*sd(disparity[[i]]/sqrt(length(disparity[[i]]))), col="Blue", lty=2)
  print(var(disparity[[i]])*10)
}
##Need to make the scales the same