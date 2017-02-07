###Confidence size

CISize$BradleyTerryGamma
suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")
BTPreds<-list()
TMPreds<-list()
for (i in suffix)
{
  BTPreds[[i]]<-sapply(CISize[[i]], FUN=function(d)d$BT)
  TMPreds[[i]]<-sapply(CISize[[i]], FUN=function(d)d$TM)
}

makeCIGraph<-function(matrices, type, ymin, ymax)
{
  suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")
  plot(1, type='n', main = paste0("Average 95% CI width ", type, " Parameters"),
       ylab="Size of Credible Interval", xlab="Week", ylim=c(ymin,ymax), xlim=c(1,13))
  for (i in 1:4)
  {
    lines(apply(matrices[[suffix[i]]], 1, mean), col=i)
  }
  legend("topright", legend=c(suffix[1], suffix[2], suffix[3], suffix[4]), col=1:4, lty=1, title="Underlying Strengths")
  
}

makeCIGraph(BTPreds, "Bradley-Terry", 3.5,5.5)
makeCIGraph(TMPreds, "Thurstone-Mosteller", 1.5,3.5)
