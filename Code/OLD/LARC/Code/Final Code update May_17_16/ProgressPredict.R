#This function takes a season of sports that is either completed or currently going and uses only
# the data prior to each week of games to predict the games of that week up to the current date.
# This function only takes five arguements and typically one requires one, they are:
#rawdata- the data of the season you wish to weekly predict the games of. This is required.
#previousrawdata- the data of the season preceeding the season you wish make weekly predictions for.
#date- the date you wish to predict up to. If left at its default it will be today's date.
#mf- the magnification factor, this should almost always be left at 1.
#adj- if something needs to be tweaked it is this and it should be either 0.1, 0.01, or 0.001. This
#       is the increment by which the mf will be changed if it needs to be changed.
#The results of this function are sixfold:
#Comparison-provides a table that give the count of the times each model predicted better than the
#       other model did as well as the number of times they tied.
#Proportion-provides the percentages based on the Comparison.
#DataTable-provides a data frame of each game's winner and who they played alongside the predicted
#       probability that team would win according to each of the models and then which team
#       predicted the actual outcome with greater accuracy.
#ProgressionTable-provides a data frame summary of how many games each model predicted better for
#       each week of the season.
#FinalTMStrenghs-gives a list of the final output strengths of each team according to the Thurtstone
#       -Mosteller Model.
#FinalBTStrenghs-gives a list of the final output strengths of each team according to the Bradley-
#       Terry Model.
progresspredict <- function(rawdata,previousrawdata=NULL,date=Sys.Date(),mf=1,adj=1) {
  new <- rawdata
  old <- previousrawdata
  N <- length(unique(c(new$Home,new$Visitor)))
  mfTM <- mf -> mfBT
  #This if statement gives strenghs to the teams for predicting the first week, either all equal
  # equal strengths of one for each team or strengths calculated from the previous season.
  if (is.null(old)) {
    OBTS <- rep(1,N)
    OTMS <- rep(1,N)
  } else {
    OldData <- dataconfigure(old)
    OBTS <- LARC.Optim(OldData,magnificationfactor=mf,adj=adj)$UpdatedStrengths
    OTMS <- LARC.Optim(OldData,ThurstoneMostellerLARC,magnificationfactor=mf,adj=adj)$UpdatedStrengths
  }
  #These set up the first time clumpof games to predict
  begin <- new$Date[1]-as.POSIXlt(new$Date[1])$wday
  Updata <- dataconfigure(new,begin+7)
  gamesbywins <- Updata$WinsVersus-matrix(0,N,N)
  #these are the empty vectors to later be filled by the data.
  weeks <- ceiling((date-begin)/7)
  ProBT <- vector()
  ProTM <- vector()
  Team <- vector()
  Versus <- vector()
  Week <- vector()
  #This double loop is where the predictions by each model are calculated and then it is looked at
  # which one is more accurate.
  for (i in 1:weeks) {
    for (j in 1:N^2) {
      if (gamesbywins[j] != 0) {
        reps <- gamesbywins[j]
        for (k in 1:reps) {
          #These are the compiling of the team matchups and the probabilities
          ProBT <- c(ProBT,
                     BTWP(OBTS[ifelse(j%%N==0,N,j%%N)],OBTS[ifelse(j%%N==0,j%/%N,j%/%N+1)],FALSE))
          ProTM <- c(ProTM,
                    TMWP(OTMS[ifelse(j%%N==0,N,j%%N)],OTMS[ifelse(j%%N==0,j%/%N,j%/%N+1)],FALSE))
          Team <- c(Team,Updata$Team[ifelse(j%%N==0,N,j%%N)])
          Versus <- c(Versus,Updata$Team[ifelse(j%%N==0,j%/%N,j%/%N+1)])
          Week <- c(Week,as.Date(begin+7*i))
        }
      }
    }
    #these all update the strengths to include the results of week that was just completed
    if (i==weeks) {break}
    Oldata <- Updata
    Updata <- dataconfigure(new,min(begin+7*(i+1),date-1))
    gamesbywins <- Updata$WinsVersus - Oldata$WinsVersus
    
    mfBT <- find.mf(Oldata,mfBT)
    mfTM <- find.mf(Oldata,mfTM,func=ThurstoneMostellerLARC)
    
    OBTS <- LARC.Optim(Oldata,magnificationfactor=mfBT,adj=adj)$UpdatedStrengths
    OTMS <- LARC.Optim(Oldata,ThurstoneMostellerLARC,magnificationfactor=mfTM,adj=adj)$UpdatedStrengths
  }
  BTP <- c(ProBT,1,0,1)
  TMP <- c(ProTM,0,1,1)
 #these and the preceeding create the tables of the data and make it so they can return a zero value
  cmprsn <- table(ifelse(BTP>TMP,"Bradley-Terry",ifelse(BTP<TMP,"Thurstone-Mosteller","Tie")))-1
  prcntgs <- prop.table(table(ifelse(BTP>TMP,"Bradley-Terry",ifelse(BTP<TMP,"Thurstone-Mosteller",
                                                                    "Tie")))-1)
  #this creates the DataTable to see each game and its correlated predictions side by side
  VisualComp <- data.frame(Week=as.Date(Week, origin = "1900-01-01")+25560,Winner=Team,Versus,
                           BradleyTerryProbabilities=BTP,ThurstoneMostellerProbabilities=TMP,
                           Model=ifelse(BTP>TMP,"Bradley-Terry",ifelse(BTP<TMP,"Thurstone-Mosteller"
                                                                       ,"Tie")),Difference=TMP-BTP)
  #these all create the totals for each week and put them into a data frame
  realweeks <- unique(VisualComp$Week)
  best <- data.frame(Week=realweeks,BradleyTerry=0,ThurstoneMosteller=0,Model="UNK")
  best$Model <- as.character(best$Model)
  for (i in realweeks) {
    temp <- table(VisualComp$Model[VisualComp$Week==i])
    best[best$Week==i,2] <- temp[1]
    best[best$Week==i,3] <- temp[2]
    best[best$Week==i,4] <- ifelse(temp[1]>temp[2],"Bradley-Terry",
                                   ifelse(temp[1]<temp[2],"Thurstone-Mosteller","Tie"))
  }
  #this compliles it into a specific list type so that instead of printing everything it only 
  # prints the comparison unless otherwise directed.
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp,
              ProgressionTable=best,FinalMStrenghs=OMS,FinalBTStrenghs=OBTS)
  class(lst) = "prthdn"
  return(lst)
}

#The following includes two examples of progresspredict, the first does not include a previous
# season's data and the other one does.
new <- datascrape("NBA")

go <- proc.time()
(pp <- progresspredict(new,mf=3))
pp$Comparison
clock <- proc.time() - go
clock

nbapt <- pp$ProgressionTable

plot(nbapt$Week,nbapt$ThurstoneMosteller)
points(nbapt$Week,nbapt$BradleyTerry,pch=3)
table(nbapt$Model)

nbapp <- pp$DataTable

old <- datascrape("NBA",TRUE,as.character(as.numeric(substr(Sys.Date(),1,4))-1))

go <- proc.time()
(ppwo <- progresspredict(new,old,mf=3))
ppwo$Comparison
clockwo <- proc.time() - go
clockwo

nbaptwo <- ppwo$ProgressionTable

plot(nbaptwo$Week,nbaptwo$ThurstoneMosteller,ylim=c(10,40))
points(nbaptwo$Week,nbaptwo$BradleyTerry,pch=3)
table(nbaptwo$Model)

nbappwo <- ppwo$DataTable

#These are some graphs comparing result using the old data and not using it
hist(nbapp$BradleyTerryProbabilities)
hist(nbappwo$BradleyTerryProbabilities)
hist(nbapp$ThurstoneMostellerProbabilities)
hist(nbappwo$ThurstoneMostellerProbabilities)
hist(nbapp$Difference)
hist(nbappwo$Difference)
