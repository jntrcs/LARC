#SimDataLoader

files<-paste0("Results/",list.files("Results/"))
type<-list()
trueStrengths<-list()
centeringValues<-list()
strengths<-list()
correlations<-list()
gameBias<-list()
weeklyGameBias<-list()


for (i in 1:length(names))
{
  load(names[i])
  type[[i]]<-dat$TrueStrengthType
  trueStrengths[[i]]<-dat$TrueStrengths
  centeringValues[[i]]<-dat$centeringValue
  strengths[[i]]<-dat$strengths
  correlations[[i]]<-dat$SpearmanCorrelation
  gameBias[[i]]<-dat$GameBias
  weeklyGameBias[[i]]<-dat$GameBiasByWeek
}
