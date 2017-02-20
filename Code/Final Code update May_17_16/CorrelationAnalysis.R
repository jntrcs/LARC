###CorrelationAnalysis.R
suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")

###CODE FOR ONE CORRELATION GRAPH--outdated



##Code for 3 correlation graphs


correlationPlot<-function(BTmatrix, TMmatrix, BTMMatrix, TMMMatrix, title, ymax=1, ymin=.2)
{
  plot(1, type='n', main = paste("Rank Correlation with", title,  "True Strengths"), ylab="Spearman Rank Correlation Coefficent",
       xlab="Week", ylim=c(ymin, ymax), xlim=c(0,14)) 
  btMean<-apply(BTmatrix, 1, mean)
  tmMean<-apply(TMmatrix, 1, mean)
  lines(btMean, lty=1, lwd=2)
  lines(tmMean, lty=1, lwd=2, col="Firebrick")
  lines(apply(BTMMatrix, 1, mean), lwd=2, lty=2, col="gray")
  lines(apply(TMMMatrix,1,mean), lwd=2, lty=2, col= "Red")
  #qboundsBT<-apply(BTmatrix, 1, FUN=function(d){quantile(d, c(0.025, .975))})
  #lines(qboundsBT[1,], lty=1, lwd=1)
  #lines(qboundsBT[2,], lty=1, lwd=1)
  #qboundsTM<-apply(TMmatrix, 1, FUN=function(d){quantile(d, c(0.025, .975))})
  #lines(qboundsTM[1,], lty=2, lwd=1, col="gray")
  #lines(qboundsTM[2,], lty=2, lwd=1, col="gray")
  #sdsBT<-apply(BTmatrix, 1, sd)
  #lowerBoundsBT<-btMean-qnorm(.975)*sdsBT/sqrt(ncol(BTmatrix))
  #lines(lowerBoundsBT, lty=2, col="Red")
  #upperBoundsBT<-btMean+qnorm(.975)*sdsBT/sqrt(ncol(BTmatrix))
  #lines(upperBoundsBT, lty=2, col="Red")
  #sdsTM<-apply(TMmatrix, 1, sd)
  #lowerBoundsTM<-tmMean-qnorm(.975)*sdsTM/sqrt(ncol(TMmatrix))
  #lines(lowerBoundsTM, lty=2, col="Blue")
  #upperBoundsTM<-tmMean+qnorm(.975)*sdsTM/sqrt(ncol(TMmatrix))
  #lines(upperBoundsTM, lty=2, col="Blue")
  legend("bottomright", legend=c("Bradley-Terry Mode", "Thurstone-Mosteller Mode", "BT Mean", "TM Mean"),
         title=paste(title, "Underlying Strengths"), lty=c(1,1,2,2), lwd=c(2), col=c("black", 
                                                                                     "firebrick", "Gray", "Red"))
}

correlationPlot<-function(BTmatrix, TMmatrix, title, ymax=1, ymin=.2)
{
  plot(1, type='n', main = paste("Rank Correlation with", title,  "True Strengths"), ylab="Spearman Rank Correlation Coefficent",
       xlab="Week", ylim=c(ymin, ymax), xlim=c(0,14)) 
  btMean<-apply(BTmatrix, 1, mean)
  tmMean<-apply(TMmatrix, 1, mean)
  lines(btMean, lty=1, lwd=2)
  lines(tmMean, lty=2, lwd=2, col="gray")
  qboundsBT<-apply(BTmatrix, 1, FUN=function(d){quantile(d, c(0.025, .975))})
  lines(qboundsBT[1,], lty=1, lwd=1)
  lines(qboundsBT[2,], lty=1, lwd=1)
  qboundsTM<-apply(TMmatrix, 1, FUN=function(d){quantile(d, c(0.025, .975))})
  lines(qboundsTM[1,], lty=2, lwd=1, col="gray")
  lines(qboundsTM[2,], lty=2, lwd=1, col="gray")
  sdsBT<-apply(BTmatrix, 1, sd)
  lowerBoundsBT<-btMean-qnorm(.975)*sdsBT/sqrt(ncol(BTmatrix))
  #lines(lowerBoundsBT, lty=2, col="Red")
  upperBoundsBT<-btMean+qnorm(.975)*sdsBT/sqrt(ncol(BTmatrix))
  #lines(upperBoundsBT, lty=2, col="Red")
  sdsTM<-apply(TMmatrix, 1, sd)
  lowerBoundsTM<-tmMean-qnorm(.975)*sdsTM/sqrt(ncol(TMmatrix))
  #lines(lowerBoundsTM, lty=2, col="Blue")
  upperBoundsTM<-tmMean+qnorm(.975)*sdsTM/sqrt(ncol(TMmatrix))
  #lines(upperBoundsTM, lty=2, col="Blue")
  legend("bottomright", legend=c("Bradley-Terry Rankings", "Thurstone-Mosteller Rankings", "95% Quantiles"),
         title=paste(title, "Underlying Strengths"), lty=c(1,2,1), lwd=c(2,2,1))
  rect(-1,-1,4,1,col = rgb(0.5,0.5,0.5,1/4), border=NA)
  legend("topleft", legend="Shaded area represents non-conference weeks", cex=.7)
}

suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")
BTMatrix<-list()
TMMatrix<-list()
for (i in suffix)
{
  BTMatrix[[i]]<-sapply(correlations[[i]], FUN = function(n) {n$BT})
  TMMatrix[[i]]<-sapply(correlations[[i]], FUN = function(n) {n$TM})
}
par(mfrow=c(1,1))

correlationPlot(BTMatrix$Beta, TMMatrix$Beta, "Beta", ymax=.7)
correlationPlot(BTMatrix$BradleyTerryGamma, TMMatrix$BradleyTerryGamma, "Bradley-Terry", ymax=.85, ymin=.4)
correlationPlot(BTMatrix$ThurstoneMostellerNormal, TMMatrix$ThurstoneMostellerNormal, "Thurstone-Mosteller",
                ymax=.95, ymin=.4)
correlationPlot(BTMatrix$ExtremeBT, TMMatrix$ExtremeBT, "Extreme-BT",
                ymax=.95, ymin=.4)
