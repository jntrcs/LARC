##Paper graphics
plot(0, type="n")
par(mfrow=c(2,2),oma = c(0, 0, 4, 0))


curve(dgamma(x, 2), xlim=c(0,6), ylab="Density", xlab="Strength", main="Bradley-Terry Shape=2")
curve(dnorm(x, 0, sd=3/4), xlim=c(-2,2),ylab="Density", xlab="Strength", main="TM Normal Mu=0, sd=0.75" )
curve(dbeta(x, 2, 2), xlim=c(0,1),ylab="Density", xlab="Strength", main="Beta Alpha=2 Beta=2" )
curve(dgamma(x, 2/3), xlim=c(0,2),ylab="Density", xlab="Strength", main="Extreme BT Shape=0.67" )
mtext("Distributions of True Strengths for Conference 6", outer = TRUE, cex = 1.5)

plot(density(finalTM[,3]), xlim=c(-3,3), xlab=expression(paste("TM ", theta)), main="Alabama Football Prior/Posterior")
curve(dnorm(x,0,1), add=T, lty=2)
legend("topleft", legend = c("Prior", "Posterior"), lty=c(2,1))

plot(density(finalTM[,42]), xlim=c(-3,3), xlab=expression(paste("TM ", theta)), main="South Caronlina Football Prior/Posterior")
curve(dnorm(x,0,1), add=T, lty=2)
legend("topleft", legend = c("Prior", "Posterior"), lty=c(2,1))

dprior<-function(x)x*exp(-x)
plot(density(finalBT[,3]), xlim=c(0,8), xlab=expression(paste("BT ", theta)), main="Alabama Football Prior/Posterior", ylim=c(0,.4))
curve(dprior(x), add=T, lty=2)
legend("topright", legend = c("Prior", "Posterior"), lty=c(2,1))

plot(density(finalBT[,42]), xlim=c(0,8), xlab=expression(paste("BT ", theta)), main="South Caronlina Football Prior/Posterior")
curve(dprior(x), add=T, lty=2)
legend("topright", legend = c("Prior", "Posterior"), lty=c(2,1))