go <- proc.time()

datascrape <- function(datatype="NBA",playoffs=FALSE,year=substr(Sys.Date(),1,4)){
  
  #this package is needed in order to scrape the data off the internet.
  library(XML)
  
  #the following lines of code scrape the data off of the website that has the URL 
  # corresponding with the chosen data type
  if(datatype=="NBA"){
    url <- paste("http://www.basketball-reference.com/leagues/NBA_",year,"_games.html",sep="")
    tables <- readHTMLTable(url)
    if (playoffs==TRUE) {
      tables[[1]] <- rbind(tables[[1]],tables[[2]])
    }
    names(tables[[1]]) <- c("Date","Start(ET)","BoxScore","Visitor","VPTS","Home",
                            "HPTS","OT?","Notes")
    tables[[1]]$Day <- substr(tables[[1]]$Date,1,3)
    tables[[1]]$Date <- as.Date(gsub(",","",substr(tables[[1]]$Date,6,
                                                   nchar(as.character(tables[[1]]$Date)))), "%b %d %Y")
  }
  if(datatype=="NHL"){
    url <- paste("http://www.hockey-reference.com/leagues/NHL_",year,"_games.html",sep="")
    tables <- readHTMLTable(url)
    if (playoffs==TRUE) {
      tables[[1]] <- rbind(tables[[1]],tables[[2]])
    }
    names(tables[[1]]) <- c("Date","Visitor","VPTS","Home","HPTS","OT?"," ","Attendance",
                            "LOG","Notes1","Notes2")
    tables[[1]]$Date <- as.Date(tables[[1]]$Date, "%Y-%m-%d")
    tables[[1]]$Day <- substr(weekdays(tables[[1]]$Date),1,3)
  }
  if(datatype=="NFL") {
    url <- paste("http://www.pro-football-reference.com/years/",year,"/games.htm",sep="")
    tables <- readHTMLTable(url)
    tables[[1]] <- tables[[1]][!(tables[[1]]$Date=="Date" | tables[[1]]$Date=="Playoffs"),]
    if(playoffs==FALSE) {
      tables[[1]] <- tables[[1]][!as.numeric(tables[[1]]$Week) > 18,]
    }
    names(tables[[1]]) <- c("Week","Day","Date","BoxScore","Winner","Location","Loser",
                            "WPTS","LPTS","WYDS","WTO","LYDS","LTO")
    tables[[1]]$Visitor <- ifelse(tables[[1]]$Location == "@",as.character(tables[[1]]$Winner),
                                  as.character(tables[[1]]$Loser))
    tables[[1]]$Home <- ifelse(tables[[1]]$Location == "@",as.character(tables[[1]]$Loser),
                               as.character(tables[[1]]$Winner))
    tables[[1]]$VPTS <- ifelse(tables[[1]]$Location == "@",as.numeric(as.character(tables[[1]]$WPTS)),
                               as.numeric(as.character(tables[[1]]$LPTS)))
    tables[[1]]$HPTS <- ifelse(tables[[1]]$Location == "@",as.numeric(as.character(tables[[1]]$LPTS)),
                               as.numeric(as.character(tables[[1]]$WPTS)))
    tables[[1]]$Date <- ifelse(Sys.Date() > as.Date(tables[[1]]$Date,"%B %d"),
                               as.Date(paste(tables[[1]]$Date,
                                             as.numeric(substr(Sys.Date(),1,4))),"%B %d %Y")
                               ,as.Date(paste(tables[[1]]$Date,as.numeric(
                                 substr(Sys.Date(),1,4))-1),"%B %d %Y"))
    tables[[1]]$Date <- as.Date(tables[[1]]$Date, origin = "1970-01-01")
    tables[[1]]$`OT?` <- "UNK"
  }
  if(datatype=="NCAAB"){
    tables <- read.fwf(file=url("http://masseyratings.com/scores.php?s=284067&sub=11590&all=1&mode=3&sch=on&format=0"),
                       skip=39, n=5869, widths=c(10, 2, 24, 3, 2, 24, 3, 10, 25),
                       col.names=c("Date","Where1","Team1","PTS1","Where2","Team2","PTS2","OT?","Notes"))
    head(tables)
    table1 <- read.fwf(file=url("http://masseyratings.com/scores.php?s=284067&sub=11590&all=1&mode=3&sch=on&format=0"),
                       skip=39, n=10, widths=c(19, 2, 24, 3, 2, 24, 3, 10, 16),
                       col.names=c("Date","Where1","Team1","PTS1","Where2","Team2","PTS2","OT?","Notes"))
    head(table1,3)
    table1$PTS1 <- as.character(table1$PTS1)
    table1$PTS2 <- as.character(table1$PTS2)
    table1$Date <- gsub("<hr><pre>","",as.character(table1$Date))
    table1$OT. <- ifelse(is.na(table1$OT.),"          ",table1$OT.)
    table1$Notes <- ifelse(is.na(table1$Notes)," ",table1$Notes)
    tables[1,] <- table1[1,]#c("2015-10-30", "  ", "AK Anchorage            ", " 74", "  ",
    head(tables)#"Upper Iowa              ", " 71", "          ", " ")
    tables[[1]] <- tables
    tables[[1]]$Date <- as.Date(tables[[1]]$Date)
    tables[[1]]$Day <- substr(weekdays(tables[[1]]$Date),1,3)
    tables[[1]]$Team1 <- gsub(" ","",as.character(tables[[1]]$Team1))
    tables[[1]]$Team2 <- gsub(" ","",as.character(tables[[1]]$Team2))
    tables[[1]]$PTS1 <- as.numeric(tables[[1]]$PTS1)
    tables[[1]]$PTS2 <- as.numeric(tables[[1]]$PTS2)
    tables[[1]]$`OT?` <- gsub(" ","",as.character(tables[[1]]$`OT.`))
    for (i in 1:nrow(tables[[1]])) {
      if (length(grep("O",tables[[1]]$`OT?`[i]))==1) {
        tables[[1]]$`OT?`[i] <- tables[[1]]$`OT?`[i]
      } else {
        tables[[1]]$`OT?`[i] <- "NO"
      }
    }
    tables[[1]]$Visitor <- ifelse(tables[[1]]$Where1 == " @",as.character(tables[[1]]$Team2),
                                  as.character(tables[[1]]$Team1))
    tables[[1]]$Home <- ifelse(tables[[1]]$Where1 == " @",as.character(tables[[1]]$Team1),
                               as.character(tables[[1]]$Team2))
    tables[[1]]$VPTS <- ifelse(tables[[1]]$Where1 == " @",as.character(tables[[1]]$PTS2),
                               as.character(tables[[1]]$PTS1))
    tables[[1]]$HPTS <- ifelse(tables[[1]]$Where1 == " @",as.character(tables[[1]]$PTS1),
                               as.character(tables[[1]]$PTS2))
  }
  
  #the next few lines create a dataframe with the data that we care about, removing empty
  # and irrelevant columns
  Scoresdf <- data.frame(tables[[1]]$Date,tables[[1]]$Day,tables[[1]]$Visitor,
                         tables[[1]]$VPTS,tables[[1]]$Home,tables[[1]]$HPTS,
                         tables[[1]]$`OT?`)
  names(Scoresdf) <- c("Date","Day","Visitor","VPTS","Home","HPTS","OT?")
  
  #these lines convert each column's data into useful data types
  Scoresdf$VPTS <- as.numeric(as.character(Scoresdf$VPTS))
  Scoresdf$HPTS <- as.numeric(as.character(Scoresdf$HPTS))
  Scoresdf$Visitor <- as.character(Scoresdf$Visitor)
  Scoresdf$Home <- as.character(Scoresdf$Home)
  Scoresdf$`OT?` <- as.character(Scoresdf$`OT?`)
  
  #these two lines create new columns for the winner and lower of each game
  Scoresdf$Winner <- ifelse(Scoresdf$VPTS > Scoresdf$HPTS,Scoresdf$Visitor,Scoresdf$Home)
  Scoresdf$Loser <- ifelse(Scoresdf$VPTS < Scoresdf$HPTS,Scoresdf$Visitor,Scoresdf$Home)
  
  #this line edits the OT? column, removing blanks and making it more understandable
  Scoresdf$`OT?` <- ifelse(Scoresdf$`OT?` == "", "NO", ifelse(Scoresdf$`OT?` == "OT", "O1",
                                                              Scoresdf$`OT?`))
  return(Scoresdf)
}

dataconfigure <- function(df, reldate=Sys.Date()-1, forsim=FALSE) {
  tt <- length(unique(c(df$Home,df$Visitor)))
  
  if (forsim == TRUE) {
    reldate <- Sys.Date() + 365
  } else {
    LD <- length(which(df$Date <= reldate))
    while (is.na(df$HPTS[LD+1])) {
      reldate <- reldate - 1
      LD <- length(which(df$Date <= reldate))
    }
  }
  
  #these first lines create a new dataframe with Team, Strength, and Wins
  data <- data.frame(sort(unique(c(df$Home,df$Visitor))),rep(1,tt),
                     table(c(df$Winner[df$Date <= reldate],unique(c(df$Home,df$Visitor))))-1)
  names(data) <- c("Team","Strength","Temp","WinsTotal")
  data$Temp <- NULL
  #the rest of the lines within the function create the versus matrix by first creating
  # an empty ttxtt matrix and then filling it via a for loops.
  mm <- matrix(0, tt, tt)
  ww <- matrix(0, tt, tt)
  pstn <- 0
  for (i in data$Team){
    for (n in data$Team){
      pstn <- pstn + 1
      x <- 0
      z <- 0
      for (y in 1:length(which(df$Date <= reldate))){
        if(df$Home[y]==i && df$Visitor[y]==n){
          x <- x + 1
          if((df$HPTS[y] < df$VPTS[y]) && forsim == FALSE){
            z <- z + 1
          }
        }
        if(df$Home[y]==n && df$Visitor[y]==i){
          x <- x + 1
          if((df$HPTS[y] > df$VPTS[y]) && forsim == FALSE){
            z <- z + 1
          }
        }
      }
      mm[pstn] <- x
      if(forsim == FALSE) {
        ww[pstn] <- z
      }
    }
  }
  data$Versus <- mm
  data$WinsVersus <- ww
  data$Team <- as.character(data$Team)
  
  if (forsim == TRUE) {
    data$WinsVersus <- NULL
    data$WinsTotal <- NULL
  }
  return(data)
}

BradleyTerryLARC <- function(strengths,wins,magnificationfactor=1) {
  PI <- 1
  PIPI <- 1
  W <- vector()
  x <- 0
  for (i in 1:length(strengths)) {
    W[i] <- sum(wins[i,])
    PI <- PI*strengths[i]^(W[i]+1)
    for (j in (i+1):length(strengths)) {
      if (j < length(strengths)+1) {
        x <- x + 1
        PIPI <- PIPI*(1/(strengths[i]+strengths[j])^(wins[i,j]+wins[j,i]))*magnificationfactor
      }
    }
  }
  return(exp(-sum(strengths))*PI*PIPI)
}

MostellerLARC <- function(strengths,wins,magnificationfactor=1) {
  PIPI <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      PIPI <- PIPI*pnorm(strengths[i]-strengths[j])^wins[i,j]*magnificationfactor
    }
  }
  return(PIPI)
}

LARC.Optim <- function(df, func = BradleyTerryLARC, increment = 0.001,
                       iterations = Inf, magnificationfactor=1, adj=1) {
  st <- df$Strength
  wv <- df$WinsVersus
  
  mf <- find.mf(df,magnificationfactor,func,adj)
  comp <- func(df$Strength,df$WinsVersus,mf)#LARC.Posterior(df, func, mf = magnificationfactor, adj = adj)
  options(digits = min(which( increment*10^(0:20)==floor(increment*10^(0:20)) )) - 1)
  inc <- 0.1
  last <- Inf
  x <- 0
  while ((comp != last & x < iterations) | inc >= increment) {
    mf <- find.mf(df,mf,func,adj)
    comp <- func(df$Strength,df$WinsVersus,mf)
    last <- comp
    for (i in 1:nrow(df)) {
      df$Strength[i] <- df$Strength[i] + inc
      new <- func(df$Strength,df$WinsVersus,mf)# <- LARC.Posterior(df, func, mf = magnificationfactor,adj=adj)
      if (comp > new) {
        df$Strength[i] <- df$Strength[i] - 2*inc
        new <- func(df$Strength,df$WinsVersus,mf)# <- LARC.Posterior(df, func, mf = magnificationfactor,adj=adj)
        if (comp > new) {
          df$Strength[i] <- df$Strength[i] + inc
        } else {
          comp <- new
        }
      } else {
        comp <- new
      }
    }
    if (comp == last) {
      inc <- inc/10
    }
    x <- x + 1
  }
  return(list(UpdatedStrengths=df$Strength,MaximizedPosterior=comp,Iterations=x))
}

NBAsimdf$Strength <- LARC.Optim(NBAdf)$UpdatedStrengths

LARC.Rank <- function(df, func=BradleyTerryLARC, increment = 0.001, 
                      iterations = Inf, dgt=3, magnificationfactor=1, adj=1) {
  options(digits=dgt)
  tt <- nrow(df)
  
  if (is.null(df$WinsTotal)) {
    for (i in 1:tt) {
      df$WinsTotal[i] <- sum(df$WinsVersus[i,])
    }
  }
  
  optimized <- LARC.Optim(df, func, increment, iterations, magnificationfactor,adj=adj)
  df$UpdatedStrength <- round(optimized$UpdatedStrengths,dgt)
  
  TempOrder <- df[order(-df$UpdatedStrength,-df$WinsTotal),]
  Ranked <- data.frame(1:tt,TempOrder$Team,TempOrder$UpdatedStrength,TempOrder$WinsTotal)
  names(Ranked) <- c("Rank","Team","Strength","WinsTotal")
  
  Ranking <- data.frame(Ranked$Rank,as.character(Ranked$Team),Ranked$Strength,Ranked$WinsTotal)
  names(Ranking) <- c("Rank","Team","Strength","WinsTotal")
  return(Ranking)
}

BTWP <- function(team1strength, team2strength, statement = TRUE, team1 = NULL, team2 = NULL) {
  prob <- team1strength/(team1strength + team2strength)
  if (statement == TRUE) {
    if (is.null(team1) == FALSE & is.null(team2) == FALSE) {
      return(cat("The ",team1," have a probability of ",prob," of beating the ",team2,".",sep=""))
    }
    if (is.null(team1) == FALSE & is.null(team2) == TRUE) {
      return(cat("The ",team1," have a probability of ",prob," of winning.",sep=""))
    }
    if (is.null(team1) == TRUE & is.null(team2) == TRUE) {
      return(cat("The probability of the first team winning is ",prob,".",sep=""))
    }
  } else {
    return(prob)
  }
}

MWP <- function(team1strength,team2strength, statement = TRUE, team1 = NULL, team2 = NULL) {
  prob <- pnorm(team1strength - team2strength)
  if (statement == TRUE) {
    if (is.null(team1) == FALSE & is.null(team2) == FALSE) {
      return(cat("The ",team1," have a probability of ",prob," of beating the ",team2,".",sep=""))
    }
    if (is.null(team1) == FALSE & is.null(team2) == TRUE) {
      return(cat("The ",team1," have a probability of ",prob," of winning.",sep=""))
    }
    if (is.null(team1) == TRUE & is.null(team2) == TRUE) {
      return(cat("The probability of the first team winning is ",prob,".",sep=""))
    }
  } else {
    return(prob)
  }
}

find.mf <- function(df, mf = 1, func = BradleyTerryLARC, adj=1) {
  if (func(df$Strength,df$WinsVersus,mf) == 0) {
    while (func(df$Strength,df$WinsVersus,mf) == 0) {
      mf <- mf + adj
    }
    mfo <- round(mf*10)
    while (func(df$Strength,df$WinsVersus,mfo) == Inf) {
      mfo <- mfo - adj
    }
    mf <- mean(c(mf,mfo))
  } else { 
    if (func(df$Strength,df$WinsVersus,mf) == Inf) {
      while (func(df$Strength,df$WinsVersus,mf) == Inf) {
        mf <- mf - adj
      }
      mfo <- round(mf/10)
      while (func(df$Strength,df$WinsVersus,mfo) == 0) {
        mfo <- mfo + adj
      }
      mf <- mean(c(mf,mfo))
    }
  }
  return(mf)
}

LARC.Posterior <- function(df, func = BradleyTerryLARC, mf=1, adj=1, report=FALSE) {
  posterior <- func(df$Strength,df$WinsVersus,mf)
  while (posterior == 0) {
    mf <- mf + adj
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  while (posterior == Inf) {
    mf <- mf - adj
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  if (report==TRUE){
    return(c(posterior,mf))
  } else {
    return(posterior)
  }
}

LARC.Prob <- function(df, random=NULL, type = "BT") {
  N <- nrow(df)
  #Gets the random draws if needed and creates the loop parameters
  if (is.null(random)==FALSE) {
    samp <- sample(1:N^2, random, FALSE)
  } else {
    samp <- 1:N^2
  }
  probs <- vector()
  #loops but only does calculations if any games are played, ignores values of zero
  for (i in samp) {
    if (df$WinsVersus[i] != 0) {
      reps <- df$WinsVersus[i]
      #removes the game
      df$WinsVersus[i] <- df$WinsVersus[i] - 1
      #chooses either "BT" or "M" based on the arguements
      if (type == "BT") {
        US <- LARC.Optim(df)$UpdatedStrengths
        #if that match up happened more than once it replicates it in the data to show that
        for (j in 1:reps) {
          probs <- c(probs,
                     BTWP(US[ifelse(i%%N==0,N,i%%N)],US[ifelse(i%%N==0,i%/%N,i%/%N+1)],FALSE))
        }
      } else {
        US <- LARC.Optim(df, MostellerLARC)$UpdatedStrengths
        for (j in 1:reps) {
          probs <- c(probs,
                     MWP(US[ifelse(i%%N==0,N,i%%N)],US[ifelse(i%%N==0,i%/%N,i%/%N+1)],FALSE))
        }
      }
      #reinputs the removed game for the next loop
      df$WinsVersus[i] <- df$WinsVersus[i] + 1
    }
  }
  return(probs)
}

LARC.Compare <- function(df) {
  BTP <- c(LARC.Prob(df, type = "BT"),0,1,0.5)
  MP <- c(LARC.Prob(df, type = "M"),1,0,0.5)
  cmprsn <- table(ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")))-1
  prcntgs <- prop.table(cmprsn)
  ss <- sum(df$WinsVersus)
  BTP <- BTP[1:ss]
  MP <- MP[1:ss]
  Team <- vector()
  Versus <- vector()
  N <- nrow(df)
  for (i in 1:N^2) {
    if (df$WinsVersus[i] != 0) {
      reps <- df$WinsVersus[i]
      for (j in 1:reps) {
        Team <- c(Team,df$Team[ifelse(i%%N==0,N,i%%N)])
        Versus <- c(Versus,df$Team[ifelse(i%%N==0,i%/%N,i%/%N+1)])
      }
    }
  }
  VisualComp <- data.frame(Winner=Team,Versus,BradleyTerryProbabilities=BTP,MostellerProbabilities=MP,
                           Model=ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")),
                           Difference=MP-BTP)
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp)
  class(lst) = "prthdn"
  return(lst)
}

print.prthdn = function(x, ...){
  x = x["Proportion"]
  NextMethod()
}

simulate <- function(df, prob.f = BTWP) {
  WinsVersus <- matrix(0,nrow(df),nrow(df))
  WinsTotal <- 0
  #the two loops and if statement make sure no games are repeated from the 
  # Versus matrix
  for (i in 1:length(df$Strength)) {
    for (j in (i+1):length(df$Strength)) {
      if (j < length(df$Strength)+1) {
        #generates the probability of the first team winning
        pr <- prob.f(df$Strength[i],df$Strength[j],FALSE)
        #randomly decides who wins each game using the generated probability
        vv <- sample(c(df$Team[i],df$Team[j]),df$Versus[i,j],TRUE,prob=c(pr,1-pr))
        # adds these wins to the new WinsVersus matrix
        WinsVersus[i,j] <- table(vv)[df$Team[i]]
        WinsVersus[j,i] <- table(vv)[df$Team[j]]
      }
    }
  }
  WinsVersus[is.na(WinsVersus)] <- 0
  WinsTotal <- rowSums(WinsVersus)
  
  return(list(WinsVersus=WinsVersus,WinsTotal=WinsTotal))
}

simulation <- function(df, simulations = 1000, type = "BT", compares = FALSE) {
  N <- nrow(df)
  strengths <- rep(0, N)
  wins <- rep(0, N)
  strong <- matrix(0, N, simulations)
  vars <- rep(0, N)
  prcnt <- vector()
  for (i in 1:simulations) {
    simdf <- df
    if (type == "BT") {
      sim <- simulate(simdf)
      simdf$WinsVersus <- sim$WinsVersus
      simstrength <- LARC.Optim(simdf)
    }
    if (type == "M") {
      sim <- simulate(simdf,MWP)
      simdf$WinsVersus <- sim$WinsVersus
      simstrength <- LARC.Optim(simdf,MostellerLARC)
    }
    if (compares == TRUE) {
      if (i==1) {
        counts <- LARC.Compare(simdf)$Comparison
      } else {
        counts <- counts + LARC.Compare(simdf)$Comparison
      }
    }
    
    simdf$US <- simstrength$UpdatedStrengths
    prp <- table(c(df[order(-df$Strength),]$Team,1,0)==c(simdf[order(-simdf$US),]$Team,0,1))
    prp <- prp-1
    prcnt <- c(prcnt,prop.table(prp)[2])
    
    strong[,i] <- simstrength$UpdatedStrengths
    strengths <- ((i - 1) * strengths + simstrength$UpdatedStrengths)/i
    wins <- ((i - 1) * wins + sim$WinsTotal)/i
  }
  vars <- vector()
  for (i in 1:N) {
    vars[i] <- var(strong[i,])
  }
  bi <- strengths/mean(strengths) - df$Strength/mean(df$Strength)
  newdf <- data.frame(df$Team, strengths, wins)
  names(newdf) <- c("Team","SimulatedStrength","WinsSummary")
  if (compares == TRUE) {
    return(list(SimulatedData=newdf,AllStrengths=strong,Variance=vars,Bias=bi,PercentPerfect=prcnt
                ,Comparison=counts,Proportion=prop.table(counts)))
  } else {
    return(list(SimulatedData=newdf,AllStrengths=strong,Variance=vars,Bias=bi,PercentPerfect=prcnt))
  }
}


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
  
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp,
              ProgressionTable=best,FinalMStrenghs=OMS,FinalBTStrenghs=OBTS)
  class(lst) = "prthdn"
  return(lst)
}

NBAScores <- datascrape()
NHLScores <- datascrape("NHL")
NFLScores <- datascrape("NFL",TRUE,"2015")
NCAAScores <- datascrape("NCAAB")

NBAdf <- dataconfigure(NBAScores)
NBAsimdf <- dataconfigure(NBAScores, forsim=TRUE)
NHLdf <- dataconfigure(NHLScores)
NFLdf <- dataconfigure(NFLScores)
NCAAdf <- datascrape(NCAAScores)

NBARankingBT <- LARC.Rank(NBAdf,adj=.1)
NBARankingM <- LARC.Rank(NBAdf,MostellerLARC,adj=.1)
NHLRankingBT <- LARC.Rank(NHLdf)
NHLRankingM <- LARC.Rank(NHLdf,MostellerLARC)
NFLRankingBT <- LARC.Rank(NFLdf)
NFLRankingM <- LARC.Rank(NFLdf,MostellerLARC)

old <- datascrape("NBA",TRUE,as.character(as.numeric(substr(Sys.Date(),1,4))-1))
new <- NBAdf

#(ppwo <- progresspredict(new,old,mf=3))

proc.time() - go
