#Strength Predictor MSE analysis


suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal")
sigti<-list()
varSigti<-list()
biasSigti<-list()


#center true Strengths
trueCenter<-list()

system.time(
for (i in suffix)
{
  trueCenter[[i]]<-trueStrengths[[i]][[1]]+centeringValues[[i]][[1]] #Because every simulation has the same normalized strengths we can just use the first
  sigti[[i]]<-list(BT=list(), TM=list())
  varSigti[[i]]<-list(BT=list(), TM=list())
  biasSigti[[i]]<-list(BT=list(), TM=list())
  
  for (week in 1:13)
  {
    sigti[[i]]$BT[[week]]<- sapply(1:90, FUN=function(n)
    {
    sapply(strengths[[i]], FUN=function(d){d$BT[[week]][n]})
  }) + t(sapply(centeringValues[[i]], FUN=function(e) e))
    sigti[[i]]$TM[[week]]<- sapply(1:90, FUN=function(n)
    {
      sapply(strengths[[i]], FUN=function(d){d$TM[[week]][n]})
    })+ t(sapply(centeringValues[[i]], FUN=function(e) e))
    varSigti[[i]]$BT[[week]]<-apply(sigti[[i]]$BT[[week]], 2, FUN=var)
    varSigti[[i]]$TM[[week]]<-apply(sigti[[i]]$TM[[week]], 2, FUN=var)
    biasSigti[[i]]$BT[[week]]<-sigti[[i]]$BT[[week]]-trueCenter[[i]]
    biasSigti[[i]]$TM[[week]]<-sigti[[i]]$TM[[week]]-trueCenter[[i]]
    
  }
}
)

mse<-list()
for (i in suffix)
{
  mse[[i]]<-list(BT=list(), TM=list())
  for (week in 1:13)
  {
    mse[[i]]$BT[[week]]<-apply(sigti[[i]]$BT[[week]], 2, var)+apply(biasSigti[[i]]$BT[[week]], 2, FUN=function(s){mean(s)^2})
   mse[[i]]$TM[[week]]<-apply(sigti[[i]]$TM[[week]], 2, var)+apply(biasSigti[[i]]$TM[[week]], 2, FUN=function(s){mean(s)^2})

  }
}

plotting<-sapply(mse$BradleyTerryGamma$BT, FUN=mean)
plot(plotting)
hist(mse$BradleyTerryGamma$BT[[13]])
