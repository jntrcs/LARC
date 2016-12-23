#SimDataLoader

files<-paste0("Results/",list.files("Results/"))
type<-character()
trueStrengths<-list()
centeringValues<-list()
strengths<-list()
correlations<-list()
gameBias<-list()
weeklyGameBias<-list()


for (i in 1:length(names))
{
  load(names[i])
  type<-c(type, dat$TrueStrengthType)
  trueStrengths[[i]]<-dat$TrueStrengths
  centeringValues[[i]]<-dat$centeringValue
  strengths[[i]]<-dat$strengths
  correlations[[i]]<-dat$SpearmanCorrelation
  gameBias[[i]]<-dat$GameBias
  weeklyGameBias[[i]]<-dat$GameBiasByWeek
}
rm(dat)



cols<-as.factor(type)
plot(correlations[[1]]$BT, type='l',col=cols[1], ylim=c(.2, 1), xlab="Week", ylab="Spearman Correlation", 
     main = "Correlation with True Rankings")

lines(correlations[[1]]$TM, lty=2, col=cols[1])
for (i in 2:10)
{
  lines(correlations[[i]]$BT, type='l',col=cols[i])
  lines(correlations[[i]]$TM, lty=2, col=cols[i])
}
