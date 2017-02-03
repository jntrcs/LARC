##Paper graphics
plot(0, type="n")
par(mfrow=c(2,2),oma = c(0, 0, 4, 0))


curve(dgamma(x, 2), xlim=c(0,6), ylab="Density", xlab="Strength", main="Bradley-Terry Shape=2")
curve(dnorm(x, 0, sd=3/4), xlim=c(-2,2),ylab="Density", xlab="Strength", main="TM Normal Mu=0, sd=0.75" )
curve(dbeta(x, 2, 2), xlim=c(0,1),ylab="Density", xlab="Strength", main="Beta Alpha=2 Beta=2" )
curve(dgamma(x, 2/3), xlim=c(0,2),ylab="Density", xlab="Strength", main="Extreme BT Shape=0.67" )
mtext("Distributions of True Strengths for Conference 6", outer = TRUE, cex = 1.5)
