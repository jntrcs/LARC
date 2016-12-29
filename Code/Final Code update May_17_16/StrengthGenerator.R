##True Strength Generator

pickTMStrength<-function(mean = 0, paramB=NULL)
{
  rnorm(length(mean),mean=mean, sd=3/4)
}

getConferenceMeans<-function(useBT, params, paramsB=NULL, beta=FALSE)
{
  if (!useBT &!beta)
    rep(params, each=9)
  else if (useBT)
    rep(params, each=9)
  else
    rep(params/(params+paramsB), each=9)
    
}

pickBetaStrength<-function(paramA, paramB)
{
  rbeta(length(paramA), paramA, paramB)
}

pickBTStrength<-function(shape, paramB=NULL)
{
  rgamma(length(shape), shape=shape)
}

simHomeWin<-function(homeStrength, awayStrength, type)
{
  if (type=="BT")
   rbinom(1,1, predictionPercentage(homeStrength, awayStrength, type))
  else if (type=="TM")
    rbinom(1,1, predictionPercentage(homeStrength, awayStrength, type))
  else
    rbinom(1,1, predictionPercentage(homeStrength, awayStrength, type))
}


predictionPercentage<-function(homeStrength, awayStrength, type)
{
  if (type=="BT")
     homeStrength/(homeStrength+awayStrength)
  else if (type=="TM")
    pnorm(homeStrength-awayStrength)
  else
    .5+(homeStrength-awayStrength)/2
}


