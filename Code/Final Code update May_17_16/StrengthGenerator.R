##True Strength Generator

pickTMStrength<-function(mean = 0)
{
  rnorm(length(mean),mean=mean, sd=3/4)
}

getConferenceMeans<-function(useBT, params)
{
  if (!useBT)
    rep(rep(0, 10), each=9)
  else
    rep(params, each=9)
    
}

pickBTStrength<-function(shape)
{
  rgamma(length(shape), shape=shape)
}

simHomeWin<-function(homeStrength, awayStrength, type)
{
  if (type=="BT")
   rbinom(1,1, predictionPercentage(homeStrength, awayStrength, type))
  else
    rbinom(1,1, predictionPercentage(homeStrength, awayStrength, type))
}


predictionPercentage<-function(homeStrength, awayStrength, type)
{
  if (type=="BT")
     homeStrength/(homeStrength+awayStrength)
  else
    pnorm(homeStrength-awayStrength)
}

