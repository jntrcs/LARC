#Game Bias Analysis
suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal")

gameBiasMatrix<-list()
for (i in suffix)
{
  gameBiasMatrix[[i]]<-list(BT=list(), TM=list())
  gameBiasMatrix[[i]]$BT<-sapply(weeklyGameBias[[i]], FUN=function(n){n$BT.MSE})
  gameBiasMatrix[[i]]$TM<-sapply(weeklyGameBias[[i]], FUN=function(n){n$TM.MSE})
}

gameBiasGraph<-function(BTMatrix, TMMatrix)
{
  plot(1, type='n', main = "Game Bias MSE", ylab="MSE", xlab="Week", ylim=c(0,.1), xlim=c(1,13))
  BTmeans<-apply(BTMatrix, 1, mean) 
  lines(BTmeans, col="Red", type='l')
  TMmeans<-apply(TMMatrix, 1, mean)
  lines(TMmeans, col="Blue", type='l')
}
gameBiasGraph(gameBiasMatrix$BradleyTerryGamma$BT, gameBiasMatrix$BradleyTerryGamma$TM)
gameBiasGraph(gameBiasMatrix$Beta$BT, gameBiasMatrix$Beta$TM)
gameBiasGraph(gameBiasMatrix$ThurstoneMostellerNormal$BT, gameBiasMatrix$ThurstoneMostellerNormal$TM)
