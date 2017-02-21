###Best Prediction Algorithm
tmLight<-which(disparity$ThurstoneMostellerNormal<.25)
btHeavy<- which(disparity$BradleyTerryGamma>.25)

suffix<-c("BradleyTerryGamma","ThurstoneMostellerNormal")
gameBiasMatrix<-list()
for (i in suffix)
{
  gameBiasMatrix[[i]]<-list(BT=list(), TM=list(), BTImproved=list(), TMImproved=list())
  gameBiasMatrix[[i]]$BT<-sapply(weeklyGameBias[[i]], FUN=function(n){n$BT.MSE})
  gameBiasMatrix[[i]]$TM<-sapply(weeklyGameBias[[i]], FUN=function(n){n$TM.MSE})
  gameBiasMatrix[[i]]$BTImproved<-sapply(1:length(weeklyGameBias[[i]]), FUN=function(j){
    if(j %in% btHeavy)
    {
      weeklyGameBias[[i]][[j]]$TM.MSE
    }
    else{
      weeklyGameBias[[i]][[j]]$BT.MSE
    }
  })
  gameBiasMatrix[[i]]$TMImproved<-sapply(1:length(weeklyGameBias[[i]]), FUN=function(j){
    if (j %in% tmLight)
    {
      weeklyGameBias[[i]][[j]]$BT.MSE
    }
    else{
      weeklyGameBias[[i]][[j]]$TM.MSE
    }
  })
}

gameBiasGraph2<-function(BTMatrix, TMMatrix,BTI, TMI, type)
{
  plot(1, type='n', main = "Win Prediction Probability MSE",
       ylab="Predicted Probability MSE", xlab="Week", ylim=c(0,.1), xlim=c(1,13), xaxt='n')
  legend("topright", legend=c("Bradley-Terry", "Thurstone-Mosteller", "BT Improved", "TM Improved"), title=paste(type, "True Strengths"),
         col=c("green", "black", "gray43", "gray43"), lty = c(1,2,1, 2), lwd=c(2))
  rect(0,-1,3,1,col = rgb(0.5,0.5,0.5,1/4), border=NA)
  legend("bottomleft", legend="Shade represents non-conference weeks")
  BTmeans<-apply(BTMatrix, 1, mean) 
  lines(BTmeans, col="Green", type='l', lwd=2)
 
  TMmeans<-apply(TMMatrix, 1, mean)

  lines(TMmeans, col="black", lwd=2, lty=2, type='l')
  
  lines(apply(BTI, 1, mean), col="gray43", lty=1, lwd=2)
  lines(apply(TMI, 1, mean), col="gray43", lty=2, lwd=2)
  axis(1,at=1:12,labels=2:13)
  
  
}

gameBiasGraph2(gameBiasMatrix$BradleyTerryGamma$BT, gameBiasMatrix$BradleyTerryGamma$TM, 
               gameBiasMatrix$BradleyTerryGamma$BTImproved, 
               gameBiasMatrix$BradleyTerryGamma$TMImproved, type = "Bradley-Terry")

gameBiasGraph2(gameBiasMatrix$ThurstoneMostellerNormal$BT, gameBiasMatrix$ThurstoneMostellerNormal$TM, 
               gameBiasMatrix$ThurstoneMostellerNormal$BTImproved, 
               gameBiasMatrix$ThurstoneMostellerNormal$TMImproved, type = "Thurstone-Mosteller")
