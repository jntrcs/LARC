##True Strength Generator

pickTMStrength<-function(mean = 0)
{
  rnorm(1,mean=mean)
}

pickBTStrength<-function(shape)
{
  rgamma(1, shape=shape)
}

simHomeWin<-function(homeStrength, awayStrength, type)
{
  if (type=="BT")
   rbinom(1,1, homeStrength/(homeStrength+awayStrength))
  else
    rbinom(1,1, pnorm(homeStrength-awayStrength))
}
