
old <- datascrape("NBA",TRUE,as.character(as.numeric(substr(Sys.Date(),1,4))-1))
new <- datascrape("NBA")

progresspredict <- function(rawdata,previousrawdata=NULL,date=Sys.Date(),mf=1) {
  new <- rawdata
  old <- previousrawdata
  N <- length(unique(c(new$Home,new$Visitor)))
  mfM <- mf -> mfBT
  
  if (is.null(old)) {
    OBTS <- rep(1,N)
    OMS <- rep(1,N)
  } else {
    OldData <- dataconfigure(old)
    OBTS <- LARC.Optim(OldData,magnificationfactor=mf)$UpdatedStrengths
    OMS <- LARC.Optim(OldData,MostellerLARC,magnificationfactor=mf)$UpdatedStrengths
  }
  
  begin <- new$Date[1]-as.POSIXlt(new$Date[1])$wday
  Updata <- dataconfigure(new,begin+7)
  gamesbywins <- Updata$WinsVersus-matrix(0,N,N)
  
  weeks <- ceiling((date-begin)/7)
  ProBT <- vector()
  ProM <- vector()
  Team <- vector()
  Versus <- vector()
  Week <- vector()
  
  for (i in 1:weeks) {
    for (j in 1:N^2) {
      if (gamesbywins[j] != 0) {
        reps <- gamesbywins[j]
        for (k in 1:reps) {
          ProBT <- c(ProBT,
                     BTWP(OBTS[ifelse(j%%N==0,N,j%%N)],OBTS[ifelse(j%%N==0,j%/%N,j%/%N+1)],FALSE))
          ProM <- c(ProM,
                    MWP(OMS[ifelse(j%%N==0,N,j%%N)],OMS[ifelse(j%%N==0,j%/%N,j%/%N+1)],FALSE))
          Team <- c(Team,Updata$Team[ifelse(j%%N==0,N,j%%N)])
          Versus <- c(Versus,Updata$Team[ifelse(j%%N==0,j%/%N,j%/%N+1)])
          Week <- c(Week,as.Date(begin+7*i))
        }
      }
    }
    if (i==weeks) {break}
    Oldata <- Updata
    Updata <- dataconfigure(new,min(begin+7*(i+1),date-1))
    gamesbywins <- Updata$WinsVersus - Oldata$WinsVersus
    
    mfBT <- find.mf(Oldata,mfBT)
    mfM <- find.mf(Oldata,mfM,func=MostellerLARC)
    
    OBTS <- LARC.Optim(Oldata,magnificationfactor=mfBT)$UpdatedStrengths
    OMS <- LARC.Optim(Oldata,MostellerLARC,magnificationfactor=mfM)$UpdatedStrengths
  }
  BTP <- c(ProBT,1,0,1)
  MP <- c(ProM,0,1,1)
  
  cmprsn <- table(ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")))-1
  prcntgs <- prop.table(table(ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")))-1)
  
  VisualComp <- data.frame(Week=as.Date(Week, origin = "1900-01-01")+25560,Winner=Team,Versus,BradleyTerryProbabilities=BTP,MostellerProbabilities=MP,
                           Model=ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")),
                           Difference=MP-BTP)
  
  realweeks <- unique(VisualComp$Week)
  best <- data.frame(Week=realweeks,BradleyTerry=0,Mosteller=0,Model="UNK")
  best$Model <- as.character(best$Model)
  for (i in realweeks) {
    temp <- table(VisualComp$Model[VisualComp$Week==i])
    best[best$Week==i,2] <- temp[1]
    best[best$Week==i,3] <- temp[2]
    best[best$Week==i,4] <- ifelse(temp[1]>temp[2],"Bradley-Terry",
                                        ifelse(temp[1]<temp[2],"Mosteller","Tie"))
  }
  
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp,
              ProgressionTable=best,FinalMStrenghs=OMS,FinalBTStrenghs=OBTS)
  class(lst) = "prthdn"
  return(lst)
}

go <- proc.time()
(pp <- progresspredict(new,mf=3))
pp$Comparison
clock <- proc.time() - go
clock

nbapt <- pp$ProgressionTable

plot(nbapt$Week,nbapt$Mosteller)
points(nbapt$Week,nbapt$BradleyTerry,pch=3)
table(nbapt$Model)

nbapp <- pp$DataTable
View(nbapp)

go <- proc.time()
(ppwo <- progresspredict(new,old,mf=3))
ppwo$Comparison
clockwo <- proc.time() - go
clockwo

nbaptwo <- ppwo$ProgressionTable

plot(nbaptwo$Week,nbaptwo$Mosteller,ylim=c(10,40))
points(nbaptwo$Week,nbaptwo$BradleyTerry,pch=3)
table(nbaptwo$Model)

nbappwo <- ppwo$DataTable
View(nbappwo)

hist(nbapp$BradleyTerryProbabilities)
hist(nbappwo$BradleyTerryProbabilities)
hist(nbapp$MostellerProbabilities)
hist(nbappwo$MostellerProbabilities)
hist(nbapp$Difference)
hist(nbappwo$Difference)

old <- datascrape("NHL",TRUE,as.character(as.numeric(substr(Sys.Date(),1,4))-1))
new <- datascrape("NHL")

go <- proc.time()
(pp <- progresspredict(new,mf=3))
pp$Comparison
clock <- proc.time() - go
clock

nhlpt <- pp$ProgressionTable

plot(nhlpt$Week,nhlpt$Mosteller)
points(nhlpt$Week,nhlpt$BradleyTerry,pch=3)
table(nhlpt$Model)

nhlpp <- pp$DataTable
View(nhlpp)

go <- proc.time()
(ppwo <- progresspredict(new,old,mf=3))
ppwo$Comparison
clockwo <- proc.time() - go
clockwo

nhlptwo <- ppwo$ProgressionTable

plot(nhlptwo$Week,nhlptwo$Mosteller,ylim=c(10,40))
points(nhlptwo$Week,nhlptwo$BradleyTerry,pch=3)
table(nhlptwo$Model)

nhlppwo <- ppwo$DataTable
View(nhlppwo)

hist(nhlpp$BradleyTerryProbabilities)
hist(nhlppwo$BradleyTerryProbabilities)
hist(nhlpp$MostellerProbabilities)
hist(nhlppwo$MostellerProbabilities)
hist(nhlpp$Difference)
hist(nhlppwo$Difference,main="Histogram of Differences between Mosteller and Bradley-Terry Predictions")

#dates <- unique(nhlppwo$Week)
#best <- data.frame(dates,`Bradley-Terry`=0,Mosteller=0,Model="UNK")
#best$Model <- as.character(best$Model)
#head(best)
#for (i in dates) {
#  temp <- table(nhlppwo$Model[nhlppwo$Week==i])
#  best[best$dates==i,2] <- temp[1]
#  best[best$dates==i,3] <- temp[2]
#  best[best$dates==i,4] <- ifelse(temp[1]>temp[2],"Bradley-Terry",ifelse(temp[1]<temp[2],"Mosteller","Tie"))
#}
#table(best$Model)

#plot(best$dates,best$Mosteller)
#points(best$dates,best$Bradley.Terry,pch=3)

#legend("left",legend=c("Negative Values indicate Bradley-Terry Predicted Better"))#,"      V      "))
#legend("right",legend=c("Positive Values indicate Mostller Predicted Better"))#,"                                     V"))

#qplot(nhlppwo$Difference, geom="histogram",bins=30)
#ggplot(data=nhlppwo, aes(nhlppwo$MostellerProbabilities)) + ggplot(data=nhlppwo, aes(nhlppwo$BradleyTerryProbabilities)) + geom_histogram()

#dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))
#type <- c(rep("BT",length(nhlppwo$BradleyTerryProbabilities)),rep("M",length(nhlppwo$MostellerProbabilities)))
#Probos <- c(nhlppwo$BradleyTerryProbabilities,nhlppwo$MostellerProbabilities)
#ploter <- data.frame(type,Probos)
#head(ploter)

#ggplot(ploter,aes(x=Probos,fill=Probos)) + 
#  geom_histogram(data=subset(ploter,type == 'BT'),fill = "red", alpha = 0.2) +
#  geom_histogram(data=subset(ploter,type == 'M'),fill = "blue", alpha = 0.2) +
#  labs(title="Overlay Histogram of NHL Win Predictions",x="Probabilities")

#typba <- c(rep("BT",length(nbappwo$BradleyTerryProbabilities)),rep("M",length(nbappwo$MostellerProbabilities)))
#Probas <- c(nbappwo$BradleyTerryProbabilities,nbappwo$MostellerProbabilities)
#plotter <- data.frame(typba,Probas)
#head(plotter)

#ggplot(plotter,aes(x=Probas,fill=Probas)) + 
#  geom_histogram(data=subset(plotter,typba == 'BT'),fill = "black", alpha = 0.2) +
#  geom_histogram(data=subset(plotter,typba == 'M'),fill = "blue", alpha = 0.2) + 
#  labs(title="Overlay Histogram of NBA Win Predictions",x="Probabilities") +
#  theme(axis.title.x=element_text(size=14),axis.title.y=element_text(size=14),title=element_text(size=16))

#plot(nbappwo$Week+25560,nbappwo$BradleyTerryProbabilities)
#abline(line(nbappwo$Week+25560,nbappwo$BradleyTerryProbabilities))
#plot(nbappwo$Week+25560,nbappwo$MostellerProbabilities)
#abline(line(nbappwo$Week+25560,nbappwo$MostellerProbabilities))
