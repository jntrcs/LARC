##True Strength Generator

pickTMStrength<-function(mean = 0)
{
  rnorm(length(mean),mean=mean, sd=3/4)
}

pickBTStrength<-function(shape)
{
  rgamma(length(shape), shape=shape)
}

simHomeWin<-function(homeStrength, awayStrength, type)
{
  if (type=="BT")
   rbinom(1,1, homeStrength/(homeStrength+awayStrength))
  else
    rbinom(1,1, pnorm(homeStrength-awayStrength))
}
