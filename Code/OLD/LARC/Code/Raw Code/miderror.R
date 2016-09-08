progresspredict <- function(rawdata,previousrawdata=NULL,date=Sys.Date(),mf=1) {
  new <- rawdata
  old <- previousrawdata
  N <- length(unique(c(new$Home,new$Visitor)))
  mfM <- mf -> mfBT
  
  if (is.null(old)) {
    OBTS <- rep(1,30)
    OMS <- rep(1,30)
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
  
  i <- 1
  
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
    
    i <- i + 1
    #rbind(ProM,Team,Versus)
    i
    View(LARC.Rank(Oldata,MostellerLARC,magnificationfactor=mfM))
    
  BTP <- ProBT
  MP <- ProM
  
  cmprsn <- table(ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")))
  prcntgs <- prop.table(table(ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie"))))
  
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
  
  plot(nbaptwo$Week,VisualComp$Mosteller)
  points(nbaptwo$Week,VisualComp$BradleyTerry,pch=3)
  table(nbaptwo$Model)
  
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp,
              ProgressionTable=best)
  class(lst) = "prthdn"
  return(lst)
}