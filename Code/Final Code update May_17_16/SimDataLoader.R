#SimDataLoader

files<-paste0("Results/",list.files("Results/"))
type<-factor()
trueStrengths<-list(BradleyTerryGamma=list(),Uniform=list(), ThurstoneMostellerNormal=list())
centeringValues<-list()
strengths<-list()
correlations<-list()
gameBias<-list()
weeklyGameBias<-list()


for (i in 1:length(files))
{
  load(files[i])
  type<-c(type, dat$TrueStrengthType)
  trueStrengths[dat$TrueStrengthType][[length(trueStrengths[dat$TrueStrengthType])]]<-dat$TrueStrengths
  centeringValues[[i]]<-dat$centeringValue
  strengths[[i]]<-dat$strengths
  correlations[[i]]<-dat$SpearmanCorrelation
  gameBias[[i]]<-dat$GameBias
    weeklyGameBias[[i]]<-dat$GameBiasByWeek
        
  }
rm(dat)
type<-as.factor(type)


corDifs<-matrix(0, nrow=length(correlations), ncol=13)
for (i in 1:length(correlations))
{
  corDifs[i,]<-correlations[[i]]$BT-correlations[[i]]$TM 
}
apply(corDifs, 2, FUN = mean)
apply(corDifs, 2, FUN = function(i)quantile(i,c(.05,.95)))


##Sample

cols<-as.factor(type)
plot(correlations[[1]]$BT, type='l',col=cols[1], ylim=c(.2, 1), xlab="Week", ylab="Spearman Correlation", 
     main = "Correlation with True Rankings")

lines(correlations[[1]]$TM, lty=2, col=cols[1])
for (i in 2:10)
{
  lines(correlations[[i]]$BT, type='l',col=cols[i])
  lines(correlations[[i]]$TM, lty=2, col=cols[i])
}
