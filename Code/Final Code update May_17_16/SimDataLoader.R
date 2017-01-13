#SimDataLoader
load("MasterFunctionFile.RData")
files<-paste0("Results/",list.files("Results/"))
type<-factor()
trueStrengths<-list(BradleyTerryGamma=vector("list", 5),Beta=vector("list", 5), ThurstoneMostellerNormal=vector("list", 5), ExtremeBT=vector("list",5))
centeringValues<-list(BradleyTerryGamma=vector("list", 5),Beta=vector("list", 5), ThurstoneMostellerNormal=vector("list", 5), ExtremeBT=vector("list",5))
strengths<-list(BradleyTerryGamma=vector("list", 5),Beta=vector("list", 5), ThurstoneMostellerNormal=vector("list", 5), ExtremeBT=vector("list",5))
correlations<-list(BradleyTerryGamma=vector("list", 5),Beta=vector("list", 5), ThurstoneMostellerNormal=vector("list", 5), ExtremeBT=vector("list",5))
gameBias<-list(BradleyTerryGamma=vector("list", 5),Beta=vector("list", 5), ThurstoneMostellerNormal=vector("list", 5), ExtremeBT=vector("list",5))
weeklyGameBias<-list(BradleyTerryGamma=vector("list", 5),Beta=vector("list", 5), ThurstoneMostellerNormal=vector("list", 5), ExtremeBT=vector("list",5))
disparity<-list(BradleyTerryGamma=numeric(),Beta=numeric(), ThurstoneMostellerNormal=numeric(), ExtremeBT=numeric())

indices<-rep(0,4) # BT then TM then Beta then EXTREMEBETA
for (i in 1:length(files))
{
  load(files[i])
  ifelse(dat$TrueStrengthType=="BradleyTerryGamma", index<-1,
         ifelse(dat$TrueStrengthType=="ThurstoneMostellerNormal", index<-2, ifelse(dat$TrueStrengthType=="Beta",index<-3, index<-4)))
  indices[index]<-indices[index]+1
  a<-indices[index]
  type<-c(type, dat$TrueStrengthType)
  trueStrengths[dat$TrueStrengthType][[1]][[a]]<-dat$TrueStrengths
  centeringValues[dat$TrueStrengthType][[1]][[a]]<-dat$centeringValue
  strengths[dat$TrueStrengthType][[1]][[a]]<-dat$strengths
  correlations[dat$TrueStrengthType][[1]][[a]]<-dat$SpearmanCorrelation
  gameBias[dat$TrueStrengthType][[1]][[a]]<-dat$GameBias
    weeklyGameBias[dat$TrueStrengthType][[1]][[a]]<-dat$GameBiasByWeek
    disparity[[dat$TrueStrengthType]][[a]]<-dat$disparityScore
        
  }
rm(dat, a, i, index, indices)
type<-as.factor(type)



