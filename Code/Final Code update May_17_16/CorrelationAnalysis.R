###Analysis.R
load("SimulationResults.RData")
suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")

###CODE FOR ONE CORRELATION GRAPH--outdated
corDif<-list()
for (i in suffix)
{
  corDif[[i]]<-sapply(correlations[[i]], FUN=function(n) {n$BT-n$TM})
}

colors<-rainbow(3)
plot(1,xlim=c(1, 13), type='n', ylim=c(-.03, .05),xlab="Week", ylab="Difference (BT-TM)", main="Rank Correlation Difference", )
abline(b=0,a=0, col="Black")
legend("topright", col=colors, legend=c("Bradley-Terry","Beta", "Thurstone Mosteller"), lty=1, title="Underlying True Strengths")
inc<-0
lapply(corDif, FUN=function(a){
inc<<-inc+1
confidence<-apply(a, 1, FUN=function(b){quantile(b,c(.1, .9))})
center<-apply(a, 1, mean)
lines(center, col=colors[inc])
#lines(confidence[1,], lty=2, col="Gray")
#lines(confidence[2,], lty=2, col="Gray")
})


##Code for 3 correlation graphs


correlationPlot<-function(BTmatrix, TMmatrix, title, ymax=1, ymin=.2)
{
 plot(1, type='n', main = paste("Rank Correlation with", title,  "True Strengths"), ylab="Spearman Rank Correlation Coefficent",
      xlab="Week", ylim=c(ymin, ymax), xlim=c(0,135)) 
  btMean<-apply(BTmatrix, 1, mean)
  tmMean<-apply(TMmatrix, 1, mean)
  lines(btMean, col="Red")
  lines(tmMean, col="Blue")
  qboundsBT<-apply(BTmatrix, 1, FUN=function(d){quantile(d, c(0.025, .975))})
  lines(qboundsBT[1,], col="pink")
  lines(qboundsBT[2,], col="pink")
  qboundsTM<-apply(TMmatrix, 1, FUN=function(d){quantile(d, c(0.025, .975))})
  lines(qboundsTM[1,], col="lightblue")
  lines(qboundsTM[2,], col="lightblue")
  sdsBT<-apply(BTmatrix, 1, sd)
  lowerBoundsBT<-btMean-qnorm(.975)*sdsBT/sqrt(ncol(BTmatrix))
  lines(lowerBoundsBT, lty=2, col="Red")
  upperBoundsBT<-btMean+qnorm(.975)*sdsBT/sqrt(ncol(BTmatrix))
  lines(upperBoundsBT, lty=2, col="Red")
  sdsTM<-apply(TMmatrix, 1, sd)
  lowerBoundsTM<-tmMean-qnorm(.975)*sdsTM/sqrt(ncol(TMmatrix))
  lines(lowerBoundsTM, lty=2, col="Blue")
  upperBoundsTM<-tmMean+qnorm(.975)*sdsTM/sqrt(ncol(TMmatrix))
  lines(upperBoundsTM, lty=2, col="Blue")
  legend("bottomright", legend=c("Bradley-Terry Rankings", "Thurstone-Mosteller Rankings", "95% Confidence Interval", "95% Quantiles"),
         title=paste(title, "Underlying Strengths"), lty=c(1,1,2,1), col=c("Red", "Blue", "Red", "pink"))
}

suffix<-c( "Beta","BradleyTerryGamma","ThurstoneMostellerNormal", "ExtremeBT")
BTMatrix<-list()
TMMatrix<-list()
for (i in suffix)
{
  BTMatrix[[i]]<-sapply(correlations[[i]], FUN = function(n) {n$BT})
  TMMatrix[[i]]<-sapply(correlations[[i]], FUN = function(n) {n$TM})

}
par(mfrow=c(1,1))

correlationPlot(BTMatrix$Beta, TMMatrix$Beta, "Beta", ymax=1)
correlationPlot(BTMatrix$BradleyTerryGamma, TMMatrix$BradleyTerryGamma, "Bradley-Terry", ymax=1, ymin=.4)
correlationPlot(BTMatrix$ThurstoneMostellerNormal, TMMatrix$ThurstoneMostellerNormal, "Thurstone-Mosteller",
                ymax=1, ymin=.4)
correlationPlot(BTMatrix$ExtremeBT, TMMatrix$ExtremeBT, "Extreme-BT",
                ymax=1, ymin=.4)

