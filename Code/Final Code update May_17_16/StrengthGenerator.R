##True Strength Generator

pickTMStrength<-function(mean = 0)
{
  rnorm(length(mean),mean=mean, sd=3/4)
}

getConferenceMeans<-function(useBT, params, uniform=FALSE)
{
  if (!useBT &!uniform)
    rep(rep(0, 10), each=9)
  else if (useBT)
    rep(params, each=9)
  else
    rep(.5, 9)
    
}

pickUnifStrength<-function(upper)
{
  runif(length(upper), upper-1, upper)
}

pickBTStrength<-function(shape)
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


