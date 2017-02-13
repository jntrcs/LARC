#Brier Score Anaylsis
library(plyr)
suffix<-c("BradleyTerryGamma", "Beta","ThurstoneMostellerNormal", "ExtremeBT")
gameBiasDF<-list()
brierScores<-list()
for (i in suffix)
{
  gameBiasDF[[i]]<-ldply(gameBias[[i]])
  brierScores[[i]]<-ddply(gameBiasDF[[i]], "Week", .fun=function(df)sapply(2:5, FUN=function(j)sum((df[,j]-df$ActualGame)^2)/nrow(df)))
  names(brierScores[[i]])<-c("Week", "BTMode", "BTMean", "TMMode", "TMMean")
}
head(brierScores$Beta)

makeBrierPlot<-function(df, type)
{
  plot(1, type='n', xlim=c(1,14), ylim=c(0,.1), main=paste0("Brier scores for ", type, " True Strengths"), xlab="Week", ylab="Brier Score")
  sapply(2:5, FUN = function(i)lines(df[,i]~df$Week, lty=ifelse(i%%2==0, 1, 2), col=ifelse(i<4, "Black", "Gray"), lwd=3))
  legend("topright", legend=names(df[,2:5]), lty=c(1,2,1,2), col=c("Black", "Black", "Gray", "gray"), lwd=3)
}
makeBrierPlot(brierScores$Beta, "Beta")

seasonBrier<-list()
for (i in suffix)
{
  df<-ldply(gameBias[[i]])
  seasonBrier[[i]]<-sapply(2:5, FUN=function(j)sum((df[,j]-df$ActualGame)^2)/nrow(df))
  names(seasonBrier[[i]])<-c("BTMode", "BTMean", "TMMode", "TMMean")
}
