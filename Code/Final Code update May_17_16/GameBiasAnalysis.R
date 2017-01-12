#Game Bias Analysis

gameBiasGraph<-function(BTMatrix, TMMatrix, type)
{
  plot(1, type='n', main = "Win Prediction Probability MSE",
       ylab="Predicted Probability MSE", xlab="Week", ylim=c(0,.1), xlim=c(1,13), xaxt='n')
  legend("topright", legend=c("Bradley-Terry", "Thurstone-Mosteller", "95% CI"), title=paste(type, "True Strengths"),
         col=c("Red", "Blue", "firebrick1"), lty = c(1,1,2))
  BTmeans<-apply(BTMatrix, 1, mean) 
  lines(BTmeans, col="Red", type='l')
  BTsds<-apply(BTMatrix, 1, sd)
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

suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal")
gameBiasMatrix<-list()
for (i in suffix)
{
  gameBiasMatrix[[i]]<-list(BT=list(), TM=list())
  gameBiasMatrix[[i]]$BT<-sapply(weeklyGameBias[[i]], FUN=function(n){n$BT.MSE})
  gameBiasMatrix[[i]]$TM<-sapply(weeklyGameBias[[i]], FUN=function(n){n$TM.MSE})
}
par(bg="gray90")
gameBiasGraph(gameBiasMatrix$BradleyTerryGamma$BT, gameBiasMatrix$BradleyTerryGamma$TM, "Bradley-Terry")
gameBiasGraph(gameBiasMatrix$Beta$BT, gameBiasMatrix$Beta$TM, "Beta")
gameBiasGraph(gameBiasMatrix$ThurstoneMostellerNormal$BT, gameBiasMatrix$ThurstoneMostellerNormal$TM, "Thurstone-Mosteller")


suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal")
par(mfrow=c(3,1))
for (i in suffix)
{
  hist(disparity[[i]], main=paste("Disparity Score for", i))
  abline(v=mean(disparity[[i]]), col="Red")
}
##Need to make the scales the same