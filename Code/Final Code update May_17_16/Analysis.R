###Analysis.R

suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal")

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

