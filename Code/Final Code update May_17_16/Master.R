#Master File

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

print.prthdn <- function(x, ...){
  x = x["Proportion"]
  NextMethod()
}

LARC.Prob <- function(df, random=NULL, type = "BT") {
  N <- nrow(df)
  if (is.null(random)==FALSE) {
    samp <- sample(1:N^2, random, FALSE)
  } else {
    samp <- 1:N^2
  }
  probs <- vector()
  for (i in samp) {
    if (df$WinsVersus[i] != 0) {
      reps <- df$WinsVersus[i]
      df$WinsVersus[i] <- df$WinsVersus[i] - 1
      if (type == "BT") {
        US <- LARC.Optim(df)$UpdatedStrengths
        for (j in 1:reps) {
          probs <- c(probs,
                     BTWP(US[ifelse(i%%N==0,N,i%%N)],US[ifelse(i%%N==0,i%/%N,i%/%N+1)],FALSE))
        }
      } else {
        US <- LARC.Optim(df, ThurstoneMostellerLARC)$UpdatedStrengths
        for (j in 1:reps) {
          probs <- c(probs,
                     MWP(US[ifelse(i%%N==0,N,i%%N)],US[ifelse(i%%N==0,i%/%N,i%/%N+1)],FALSE))
        }
      }
      df$WinsVersus[i] <- df$WinsVersus[i] + 1
    }
  }
  return(probs)
}

datascrape <- function(datatype="NBA",playoffs=FALSE,year=substr(Sys.Date(),1,4)){
  
  library(XML)
  
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
  
  Scoresdf <- data.frame(tables[[1]]$Date,tables[[1]]$Day,tables[[1]]$Visitor,
                         tables[[1]]$VPTS,tables[[1]]$Home,tables[[1]]$HPTS,
                         tables[[1]]$`OT?`)
  names(Scoresdf) <- c("Date","Day","Visitor","VPTS","Home","HPTS","OT?")
  
  Scoresdf$VPTS <- as.numeric(as.character(Scoresdf$VPTS))
  Scoresdf$HPTS <- as.numeric(as.character(Scoresdf$HPTS))
  Scoresdf$Visitor <- as.character(Scoresdf$Visitor)
  Scoresdf$Home <- as.character(Scoresdf$Home)
  Scoresdf$`OT?` <- as.character(Scoresdf$`OT?`)
  
  Scoresdf$Winner <- ifelse(Scoresdf$VPTS > Scoresdf$HPTS,Scoresdf$Visitor,Scoresdf$Home)
  Scoresdf$Loser <- ifelse(Scoresdf$VPTS < Scoresdf$HPTS,Scoresdf$Visitor,Scoresdf$Home)
  
  Scoresdf$`OT?` <- ifelse(Scoresdf$`OT?` == "", "NO", ifelse(Scoresdf$`OT?` == "OT", "O1",
                                                              Scoresdf$`OT?`))
  return(Scoresdf)
}

dataconfigure <- function(df, reldate=Sys.Date()-1, forsim=FALSE) {
  tt <- length(unique(c(df$Home,df$Visitor)))
  
  if (forsim == TRUE) {
    reldate <- Sys.Date() + 365
  } #else {
  #LD <- length(which(df$Date <= reldate))
  #while (is.na(df$HPTS[LD+1])) {
  #  reldate <- reldate - 1
  #  LD <- length(which(df$Date <= reldate))
  #}
  #}
  
  ata <- data.frame(sort(unique(c(df$Home,df$Visitor))),rep(1,tt),
                     table(c(df$Winner[df$Date <= reldate],unique(c(df$Home,df$Visitor))))-1)
  names(data) <- c("Team","Strength","Temp","WinsTotal")
  data$Temp <- NULL
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

ThurstoneMostellerLARC <- function(strengths,wins,magnificationfactor=1) {
  
  # First we compute the Prior
  prior <- 1
  for (i in 1: length(strengths)) { prior = prior * dnorm( strengths[i])*magnificationfactor}
  # Now we compute the conditional
  cond <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      cond <- cond*pnorm(strengths[i]-strengths[j])^wins[i,j]*magnificationfactor
    }
  }
  # Now put the two together 
  post = prior * cond
  return(post)
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

TMWP <- function(team1strength,team2strength, statement = TRUE, team1 = NULL, team2 = NULL) {
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
    if (type == "TM") {
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

simulate <- function(df, prob.f = BTWP) {
  WinsVersus <- matrix(0,nrow(df),nrow(df))
  WinsTotal <- 0
  for (i in 1:length(df$Strength)) {
    for (j in (i+1):length(df$Strength)) {
      if (j < length(df$Strength)+1) {
        pr <- prob.f(df$Strength[i],df$Strength[j],FALSE)
        vv <- sample(c(df$Team[i],df$Team[j]),df$Versus[i,j],TRUE,prob=c(pr,1-pr))
        WinsVersus[i,j] <- table(vv)[df$Team[i]]
        WinsVersus[j,i] <- table(vv)[df$Team[j]]
      }
    }
  }
  WinsVersus[is.na(WinsVersus)] <- 0
  WinsTotal <- rowSums(WinsVersus)
  
  return(list(WinsVersus=WinsVersus,WinsTotal=WinsTotal))
}

sim.gen <- function(teams, games, strengths=NULL) {
  if (length(teams) > 1 && length(strengths) > 1) {
    swtch <- TRUE
  } else {
    swtch <- FALSE
  }
  if (length(teams) > 1) {
    Team <- teams
    teams <- length(teams)
  } else {
    pkmn <- sort(c("Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise", "Caterpie", "Metapod", "Butterfree", "Weedle", "Kakuna", "Beedrill", "Pidgey", "Pidgeotto", "Pidgeot", "Rattata", "Raticate", "Spearow", "Fearow", "Ekans", "Arbok", "Pichu", "Pikachu", "Raichu", "Sandshrew", "Sandslash", "NidoranM","NidoranF", "Nidorina", "Nidoqueen", "Nidorino", "Nidoking", "Cleffa", "Clefairy", "Clefable", "Vulpix", "Ninetales", "Igglybuff", "Jigglypuff", "Wigglytuff", "Zubat", "Golbat", "Crobat", "Oddish", "Gloom", "Vileplume", "Bellossom", "Paras", "Parasect", "Venonat", "Venomoth", "Diglett", "Dugtrio", "Meowth",
                   "Persian", "Psyduck", "Golduck", "Mankey", "Primeape", "Growlithe", "Arcanine", "Poliwag", "Poliwhirl", "Poliwrath", "Politoed", "Abra", "Kadabra", "Alakazam", "Machop", "Machoke", "Machamp", "Bellsprout", "Weepinbell", "Victreebel", "Tentacool", "Tentacruel", "Geodude", "Graveler", "Golem", "Ponyta", "Rapidash", "Slowpoke", "Slowbro", "Slowking", "Magnemite", "Magneton", "Magnezone", "Farfetchd", "Doduo", "Dodrio", "Seel", "Dewgong", "Grimer", "Muk", "Shellder", "Cloyster", "Gastly", "Haunter", "Gengar", "Onix", "Steelix", "Drowzee", "Hypno", "Krabby", "Kingler", "Voltorb", "Electrode", "Exeggcute", "Exeggutor", "Kangaskhan", "Cubone",
                   "Marowak", "Tyrogue", "Hitmonlee", "Hitmonchan", "Hitmontop", "Lickitung", "Lickilicky", "Koffing", "Weezing", "Rhyhorn", "Rhydon", "Rhyperior", "Happiny", "Chansey", "Blissey", "Tangela", "Tangrowth", "Horsea", "Seadra", "Kingdra", "Goldeen", "Seaking", "Staryu", "Starmie", "MimeJr", "MrMime", "Scyther", "Scizor", "Smoochum", "Jynx", "Elekid", "Electabuzz", "Electivire", "Magby", "Magmar", "Magmortar", "Pinsir", "Tauros", "Miltank", "Magikarp", "Gyarados", "Lapras", "Ditto", "Eevee", "Vaporeon", "Jolteon", "Flareon", "Espeon", "Umbreon", "Leafeon", "Glaceon", "Sylveon", "Porygon", "Porygon2", "PorygonZ", "Omanyte", "Omastar", "Kabuto",
                   "Kabutops", "Aerodactyl", "Munchlax", "Snorlax", "Articuno", "Zapdos", "Moltres", "Dratini", "Dragonair", "Dragonite", "Mewtwo", "Mew", "Chikorita", "Bayleef", "Meganium", "Cyndaquil", "Quilava", "Typhlosion", "Totodile", "Croconaw", "Feraligatr", "Sentret", "Furret", "Hoothoot", "Noctowl", "Ledyba", "Ledian", "Spinarak", "Ariados", "Chinchou", "Lanturn", "Togepi", "Togetic", "Togekiss", "Natu", "Xatu", "Mareep", "Flaaffy", "Ampharos", "Azurill", "Marill", "Azumarill", "Bonsly", "Sudowoodo", "Hoppip", "Skiploom", "Jumpluff", "Aipom", "Ambipom", "Sunkern", "Sunflora", "Yanma", "Yanmega", "Wooper", "Quagsire", "Murkrow", "Honchkrow",
                   "Misdreavus", "Mismagius", "Unown", "Wynaut", "Wobbuffet", "Girafarig", "Pineco", "Forretress", "Dunsparce", "Gligar", "Gliscor", "Snubbull", "Granbull", "Qwilfish", "Shuckle", "Heracross", "Sneasel", "Weavile", "Teddiursa", "Ursaring", "Slugma", "Magcargo", "Swinub", "Piloswine", "Mamoswine", "Corsola", "Remoraid", "Octillery", "Delibird", "Mantyke", "Mantine", "Skarmory", "Houndour", "Houndoom", "Phanpy", "Donphan", "Stantler", "Smeargle", "Raikou", "Entei", "Suicune", "Larvitar", "Pupitar", "Tyranitar", "Lugia", "HoOh", "Celebi", "Treecko", "Grovyle", "Sceptile", "Torchic", "Combusken", "Blaziken", "Mudkip", "Marshtomp", "Swampert",
                   "Poochyena", "Mightyena", "Zigzagoon", "Linoone", "Wurmple", "Silcoon", "Beautifly", "Cascoon", "Dustox", "Lotad", "Lombre", "Ludicolo", "Seedot", "Nuzleaf", "Shiftry", "Taillow", "Swellow", "Wingull", "Pelipper", "Ralts", "Kirlia", "Gardevoir", "Gallade", "Surskit", "Masquerain", "Shroomish", "Breloom", "Slakoth", "Vigoroth", "Slaking", "Nincada", "Ninjask", "Shedinja", "Whismur", "Loudred", "Exploud", "Makuhita", "Hariyama", "Nosepass", "Probopass", "Skitty", "Delcatty", "Sableye", "Mawile", "Aron", "Lairon", "Aggron", "Meditite", "Medicham", "Electrike", "Manectric", "Plusle", "Minun", "Volbeat", "Illumise", "Budew", "Roselia",
                   "Roserade", "Gulpin", "Swalot", "Carvanha", "Sharpedo", "Wailmer", "Wailord", "Numel", "Camerupt", "Torkoal", "Spoink", "Grumpig", "Spinda", "Trapinch", "Vibrava", "Flygon", "Cacnea", "Cacturne", "Swablu", "Altaria", "Zangoose", "Seviper", "Lunatone", "Solrock", "Barboach", "Whiscash", "Corphish", "Crawdaunt", "Baltoy", "Claydol", "Lileep", "Cradily", "Anorith", "Armaldo", "Feebas", "Milotic", "Castform", "Kecleon", "Shuppet", "Banette", "Duskull", "Dusclops", "Dusknoir", "Tropius", "Chingling", "Chimecho", "Absol", "Snorunt", "Glalie", "Froslass", "Spheal", "Sealeo", "Walrein", "Clamperl", "Huntail", "Gorebyss", "Relicanth", "Luvdisc",
                   "Bagon", "Shelgon", "Salamence", "Beldum", "Metang", "Metagross", "Regirock", "Regice", "Registeel", "Latias", "Latios", "Kyogre", "Groudon", "Rayquaza", "Jirachi", "Deoxys", "Turtwig", "Grotle", "Torterra", "Chimchar", "Monferno", "Infernape", "Piplup", "Prinplup", "Empoleon", "Starly", "Staravia", "Staraptor", "Bidoof", "Bibarel", "Kricketot", "Kricketune", "Shinx", "Luxio", "Luxray", "Cranidos", "Rampardos", "Shieldon", "Bastiodon", "Burmy", "Wormadam", "Mothim", "Combee", "Vespiquen", "Pachirisu", "Buizel", "Floatzel", "Cherubi", "Cherrim", "Shellos", "Gastrodon", "Drifloon", "Drifblim", "Buneary", "Lopunny", "Glameow", "Purugly",
                   "Stunky", "Skuntank", "Bronzor", "Bronzong", "Chatot", "Spiritomb", "Gible", "Gabite", "Garchomp", "Riolu", "Lucario", "Hippopotas", "Hippowdon", "Skorupi", "Drapion", "Croagunk", "Toxicroak", "Carnivine", "Finneon", "Lumineon", "Snover", "Abomasnow", "Rotom", "Uxie", "Mesprit", "Azelf", "Dialga", "Palkia", "Heatran", "Regigigas", "Giratina", "Cresselia", "Phione", "Manaphy", "Darkrai", "Shaymin", "Arceus", "Victini", "Snivy", "Servine", "Serperior", "Tepig", "Pignite", "Emboar", "Oshawott", "Dewott", "Samurott", "Patrat", "Watchog", "Lillipup", "Herdier", "Stoutland", "Purrloin", "Liepard", "Pansage", "Simisage", "Pansear", "Simisear",
                   "Panpour", "Simipour", "Munna", "Musharna", "Pidove", "Tranquill", "Unfezant", "Blitzle", "Zebstrika", "Roggenrola", "Boldore", "Gigalith", "Woobat", "Swoobat", "Drilbur", "Excadrill", "Audino", "Timburr", "Gurdurr", "Conkeldurr", "Tympole", "Palpitoad", "Seismitoad", "Throh", "Sawk", "Sewaddle", "Swadloon", "Leavanny", "Venipede", "Whirlipede", "Scolipede", "Cottonee", "Whimsicott", "Petilil", "Lilligant", "Basculin", "Sandile", "Krokorok", "Krookodile", "Darumaka", "Darmanitan", "Maractus", "Dwebble", "Crustle", "Scraggy", "Scrafty", "Sigilyph", "Yamask", "Cofagrigus", "Tirtouga", "Carracosta", "Archen", "Archeops", "Trubbish",
                   "Garbodor", "Zorua", "Zoroark", "Minccino", "Cinccino", "Gothita", "Gothorita", "Gothitelle", "Solosis", "Duosion", "Reuniclus", "Ducklett", "Swanna", "Vanillite", "Vanillish", "Vanilluxe", "Deerling", "Sawsbuck", "Emolga", "Karrablast", "Escavalier", "Foongus", "Amoonguss", "Frillish", "Jellicent", "Alomomola", "Joltik", "Galvantula", "Ferroseed", "Ferrothorn", "Klink", "Klang", "Klinklang", "Tynamo", "Eelektrik", "Eelektross", "Elgyem", "Beheeyem", "Litwick", "Lampent", "Chandelure", "Axew", "Fraxure", "Haxorus", "Cubchoo", "Beartic", "Cryogonal", "Shelmet", "Accelgor", "Stunfisk", "Mienfoo", "Mienshao", "Druddigon", "Golett", "Golurk",
                   "Pawniard", "Bisharp", "Bouffalant", "Rufflet", "Braviary", "Vullaby", "Mandibuzz", "Heatmor", "Durant", "Deino", "Zweilous", "Hydreigon", "Larvesta", "Volcarona", "Cobalion", "Terrakion", "Virizion", "Tornadus", "Thundurus", "Reshiram", "Zekrom", "Landorus", "Kyurem", "Keldeo", "Meloetta", "Genesect", "Chespin", "Quilladin", "Chesnaught", "Fennekin", "Braixen", "Delphox", "Froakie", "Frogadier", "Greninja", "Bunnelby", "Diggersby", "Fletchling", "Fletchinder", "Talonflame", "Scatterbug",
                   "Spewpa", "Vivillon", "Litleo", "Pyroar", "Flabébé", "Floette", "Florges", "Skiddo", "Gogoat", "Pancham", "Pangoro", "Furfrou", "Espurr", "Meowstic", "Honedge", "Doublade", "Aegislash", "Spritzee", "Aromatisse", "Swirlix", "Slurpuff", "Inkay", "Malamar", "Binacle", "Barbaracle", "Skrelp", "Dragalge", "Clauncher", "Clawitzer", "Helioptile", "Heliolisk", "Tyrunt", "Tyrantrum", "Amaura", "Aurorus", "Hawlucha", "Dedenne", "Carbink", "Goomy", "Sliggoo", "Goodra", "Klefki", "Phantump", "Trevenant", "Pumpkaboo", "Gourgeist", "Bergmite", "Avalugg", "Noibat", "Noivern", "Xerneas", "Yveltal", "Zygarde", "Diancie", "Hoopa", "Volcanion", "Magearna"))
    Team <- c(LETTERS[1:min(teams,26)],
              letters[ifelse(teams>26,1,0):ifelse(teams>26,min(teams-26,26),0)],
              month.abb[ifelse(teams>26,1,0):ifelse(teams>52,min(teams-52,12),0)],
              month.name[ifelse(teams>26,1,0):ifelse(teams>64,min(teams-64,12),0)],
              pkmn[ifelse(teams>26,1,0):ifelse(teams>76,min(teams-76,722),0)])
  }
  if (is.null(strengths)) {
    Strength <- seq(2,.001,length.out=teams)
  } else {
    Strength <- strengths
  }
  if (swtch == FALSE) {
    Strength <- rev(sort(Strength))
  }
  Versus <- VS.matrix(teams, games)
  spit  <- data.frame(Team,Strength)
  spit$Versus <- Versus
  spit$Team <- as.character(spit$Team)
  return(spit)
}

VS.matrix <- function(teams, games) {
  mm <- matrix(0,teams,teams)
  nogo <- vector()
  tt <- 1:teams^2
  for (i in 1:teams) {
    nogo <- c(nogo,teams*(i-1)+i)
  }
  tt <- tt[-which(tt==nogo)]
  for (i in 1:games) {
    sam <- sample(tt,1)
    mm[sam] <- mm[sam] + 1
  }
  vs <- mm+t(mm)
  return(vs)
}

progresspredict <- function(rawdata,previousrawdata=NULL,date=Sys.Date(),mf=1,adj=1) {
  new <- rawdata
  old <- previousrawdata
  N <- length(unique(c(new$Home,new$Visitor)))
  mfTM <- mf -> mfBT
  
  if (is.null(old)) {
    OBTS <- rep(1,N)
    OTMS <- rep(1,N)
  } else {
    OldData <- dataconfigure(old)
    OBTS <- LARC.Optim(OldData,magnificationfactor=mf,adj=adj)$UpdatedStrengths
    OTMS <- LARC.Optim(OldData,ThurstoneMostellerLARC,magnificationfactor=mf,adj=adj)$UpdatedStrengths
  }
  
  begin <- new$Date[1]-as.POSIXlt(new$Date[1])$wday
  Updata <- dataconfigure(new,begin+7)
  gamesbywins <- Updata$WinsVersus-matrix(0,N,N)
  
  weeks <- ceiling((date-begin)/7)
  ProBT <- vector()
  ProTM <- vector()
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
          ProTM <- c(ProTM,
                     TMWP(OTMS[ifelse(j%%N==0,N,j%%N)],OTMS[ifelse(j%%N==0,j%/%N,j%/%N+1)],FALSE))
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
    mfTM <- find.mf(Oldata,mfTM,func=ThurstoneMostellerLARC)
    
    OBTS <- LARC.Optim(Oldata,magnificationfactor=mfBT,adj=adj)$UpdatedStrengths
    OTMS <- LARC.Optim(Oldata,ThurstoneMostellerLARC,magnificationfactor=mfTM,adj=adj)$UpdatedStrengths
  }
  BTP <- c(ProBT,1,0,1)
  TMP <- c(ProTM,0,1,1)
  
  cmprsn <- table(ifelse(BTP>TMP,"Bradley-Terry",ifelse(BTP<TMP,"Thurstone-Mosteller","Tie")))-1
  prcntgs <- prop.table(table(ifelse(BTP>TMP,"Bradley-Terry",ifelse(BTP<TMP,"Thurstone-Mosteller","Tie")))-1)
  
  VisualComp <- data.frame(Week=as.Date(Week, origin = "1900-01-01")+25560,Winner=Team,Versus,BradleyTerryProbabilities=BTP,ThurstoneMostellerProbabilities=TMP,
                           Model=ifelse(BTP>TMP,"Bradley-Terry",ifelse(BTP<TMP,"Thurstone-Mosteller","Tie")),
                           Difference=TMP-BTP)
  
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
  
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp,
              ProgressionTable=best,FinalMStrenghs=OMS,FinalBTStrenghs=OBTS)
  class(lst) = "prthdn"
  return(lst)
}

full.sim.gen <- function(teams, weeks=NULL, strengths=NULL, divisions, gid, god=NULL, 
                         god.type="times", enddate=Sys.Date(), type="BT") {
  if (divisions > 1 && is.null(god)) {
    god <- 1
  }
  if (god.type != "times" & god.type != "teams") {
    god.type <- "times"
  }
  if (god.type == "times") {
    times <- god
    odt <- (teams/divisions)*(divisions-1)
  } else {
    times <- 1
    odt <- god
  }
  if (length(teams) > 1 && length(strengths) > 1) {
    swtch <- TRUE
  } else {
    swtch <- FALSE
  }
  if (length(teams) > 1) {
    Team <- teams
    teams <- length(teams)
    if (is.null(weeks)) {
      weeks <- teams
    }
  } else {
    if (is.null(weeks)) {
      weeks <- teams
    }
    pkmn <- sort(c("Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise", "Caterpie", "Metapod", "Butterfree", "Weedle", "Kakuna", "Beedrill", "Pidgey", "Pidgeotto", "Pidgeot", "Rattata", "Raticate", "Spearow", "Fearow", "Ekans", "Arbok", "Pichu", "Pikachu", "Raichu", "Sandshrew", "Sandslash", "NidoranM","NidoranF", "Nidorina", "Nidoqueen", "Nidorino", "Nidoking", "Cleffa", "Clefairy", "Clefable", "Vulpix", "Ninetales", "Igglybuff", "Jigglypuff", "Wigglytuff", "Zubat", "Golbat", "Crobat", "Oddish", "Gloom", "Vileplume", "Bellossom", "Paras", "Parasect", "Venonat", "Venomoth", "Diglett", "Dugtrio", "Meowth",
                   "Persian", "Psyduck", "Golduck", "Mankey", "Primeape", "Growlithe", "Arcanine", "Poliwag", "Poliwhirl", "Poliwrath", "Politoed", "Abra", "Kadabra", "Alakazam", "Machop", "Machoke", "Machamp", "Bellsprout", "Weepinbell", "Victreebel", "Tentacool", "Tentacruel", "Geodude", "Graveler", "Golem", "Ponyta", "Rapidash", "Slowpoke", "Slowbro", "Slowking", "Magnemite", "Magneton", "Magnezone", "Farfetchd", "Doduo", "Dodrio", "Seel", "Dewgong", "Grimer", "Muk", "Shellder", "Cloyster", "Gastly", "Haunter", "Gengar", "Onix", "Steelix", "Drowzee", "Hypno", "Krabby", "Kingler", "Voltorb", "Electrode", "Exeggcute", "Exeggutor", "Kangaskhan", "Cubone",
                   "Marowak", "Tyrogue", "Hitmonlee", "Hitmonchan", "Hitmontop", "Lickitung", "Lickilicky", "Koffing", "Weezing", "Rhyhorn", "Rhydon", "Rhyperior", "Happiny", "Chansey", "Blissey", "Tangela", "Tangrowth", "Horsea", "Seadra", "Kingdra", "Goldeen", "Seaking", "Staryu", "Starmie", "MimeJr", "MrMime", "Scyther", "Scizor", "Smoochum", "Jynx", "Elekid", "Electabuzz", "Electivire", "Magby", "Magmar", "Magmortar", "Pinsir", "Tauros", "Miltank", "Magikarp", "Gyarados", "Lapras", "Ditto", "Eevee", "Vaporeon", "Jolteon", "Flareon", "Espeon", "Umbreon", "Leafeon", "Glaceon", "Sylveon", "Porygon", "Porygon2", "PorygonZ", "Omanyte", "Omastar", "Kabuto",
                   "Kabutops", "Aerodactyl", "Munchlax", "Snorlax", "Articuno", "Zapdos", "Moltres", "Dratini", "Dragonair", "Dragonite", "Mewtwo", "Mew", "Chikorita", "Bayleef", "Meganium", "Cyndaquil", "Quilava", "Typhlosion", "Totodile", "Croconaw", "Feraligatr", "Sentret", "Furret", "Hoothoot", "Noctowl", "Ledyba", "Ledian", "Spinarak", "Ariados", "Chinchou", "Lanturn", "Togepi", "Togetic", "Togekiss", "Natu", "Xatu", "Mareep", "Flaaffy", "Ampharos", "Azurill", "Marill", "Azumarill", "Bonsly", "Sudowoodo", "Hoppip", "Skiploom", "Jumpluff", "Aipom", "Ambipom", "Sunkern", "Sunflora", "Yanma", "Yanmega", "Wooper", "Quagsire", "Murkrow", "Honchkrow",
                   "Misdreavus", "Mismagius", "Unown", "Wynaut", "Wobbuffet", "Girafarig", "Pineco", "Forretress", "Dunsparce", "Gligar", "Gliscor", "Snubbull", "Granbull", "Qwilfish", "Shuckle", "Heracross", "Sneasel", "Weavile", "Teddiursa", "Ursaring", "Slugma", "Magcargo", "Swinub", "Piloswine", "Mamoswine", "Corsola", "Remoraid", "Octillery", "Delibird", "Mantyke", "Mantine", "Skarmory", "Houndour", "Houndoom", "Phanpy", "Donphan", "Stantler", "Smeargle", "Raikou", "Entei", "Suicune", "Larvitar", "Pupitar", "Tyranitar", "Lugia", "HoOh", "Celebi", "Treecko", "Grovyle", "Sceptile", "Torchic", "Combusken", "Blaziken", "Mudkip", "Marshtomp", "Swampert",
                   "Poochyena", "Mightyena", "Zigzagoon", "Linoone", "Wurmple", "Silcoon", "Beautifly", "Cascoon", "Dustox", "Lotad", "Lombre", "Ludicolo", "Seedot", "Nuzleaf", "Shiftry", "Taillow", "Swellow", "Wingull", "Pelipper", "Ralts", "Kirlia", "Gardevoir", "Gallade", "Surskit", "Masquerain", "Shroomish", "Breloom", "Slakoth", "Vigoroth", "Slaking", "Nincada", "Ninjask", "Shedinja", "Whismur", "Loudred", "Exploud", "Makuhita", "Hariyama", "Nosepass", "Probopass", "Skitty", "Delcatty", "Sableye", "Mawile", "Aron", "Lairon", "Aggron", "Meditite", "Medicham", "Electrike", "Manectric", "Plusle", "Minun", "Volbeat", "Illumise", "Budew", "Roselia",
                   "Roserade", "Gulpin", "Swalot", "Carvanha", "Sharpedo", "Wailmer", "Wailord", "Numel", "Camerupt", "Torkoal", "Spoink", "Grumpig", "Spinda", "Trapinch", "Vibrava", "Flygon", "Cacnea", "Cacturne", "Swablu", "Altaria", "Zangoose", "Seviper", "Lunatone", "Solrock", "Barboach", "Whiscash", "Corphish", "Crawdaunt", "Baltoy", "Claydol", "Lileep", "Cradily", "Anorith", "Armaldo", "Feebas", "Milotic", "Castform", "Kecleon", "Shuppet", "Banette", "Duskull", "Dusclops", "Dusknoir", "Tropius", "Chingling", "Chimecho", "Absol", "Snorunt", "Glalie", "Froslass", "Spheal", "Sealeo", "Walrein", "Clamperl", "Huntail", "Gorebyss", "Relicanth", "Luvdisc",
                   "Bagon", "Shelgon", "Salamence", "Beldum", "Metang", "Metagross", "Regirock", "Regice", "Registeel", "Latias", "Latios", "Kyogre", "Groudon", "Rayquaza", "Jirachi", "Deoxys", "Turtwig", "Grotle", "Torterra", "Chimchar", "Monferno", "Infernape", "Piplup", "Prinplup", "Empoleon", "Starly", "Staravia", "Staraptor", "Bidoof", "Bibarel", "Kricketot", "Kricketune", "Shinx", "Luxio", "Luxray", "Cranidos", "Rampardos", "Shieldon", "Bastiodon", "Burmy", "Wormadam", "Mothim", "Combee", "Vespiquen", "Pachirisu", "Buizel", "Floatzel", "Cherubi", "Cherrim", "Shellos", "Gastrodon", "Drifloon", "Drifblim", "Buneary", "Lopunny", "Glameow", "Purugly",
                   "Stunky", "Skuntank", "Bronzor", "Bronzong", "Chatot", "Spiritomb", "Gible", "Gabite", "Garchomp", "Riolu", "Lucario", "Hippopotas", "Hippowdon", "Skorupi", "Drapion", "Croagunk", "Toxicroak", "Carnivine", "Finneon", "Lumineon", "Snover", "Abomasnow", "Rotom", "Uxie", "Mesprit", "Azelf", "Dialga", "Palkia", "Heatran", "Regigigas", "Giratina", "Cresselia", "Phione", "Manaphy", "Darkrai", "Shaymin", "Arceus", "Victini", "Snivy", "Servine", "Serperior", "Tepig", "Pignite", "Emboar", "Oshawott", "Dewott", "Samurott", "Patrat", "Watchog", "Lillipup", "Herdier", "Stoutland", "Purrloin", "Liepard", "Pansage", "Simisage", "Pansear", "Simisear",
                   "Panpour", "Simipour", "Munna", "Musharna", "Pidove", "Tranquill", "Unfezant", "Blitzle", "Zebstrika", "Roggenrola", "Boldore", "Gigalith", "Woobat", "Swoobat", "Drilbur", "Excadrill", "Audino", "Timburr", "Gurdurr", "Conkeldurr", "Tympole", "Palpitoad", "Seismitoad", "Throh", "Sawk", "Sewaddle", "Swadloon", "Leavanny", "Venipede", "Whirlipede", "Scolipede", "Cottonee", "Whimsicott", "Petilil", "Lilligant", "Basculin", "Sandile", "Krokorok", "Krookodile", "Darumaka", "Darmanitan", "Maractus", "Dwebble", "Crustle", "Scraggy", "Scrafty", "Sigilyph", "Yamask", "Cofagrigus", "Tirtouga", "Carracosta", "Archen", "Archeops", "Trubbish",
                   "Garbodor", "Zorua", "Zoroark", "Minccino", "Cinccino", "Gothita", "Gothorita", "Gothitelle", "Solosis", "Duosion", "Reuniclus", "Ducklett", "Swanna", "Vanillite", "Vanillish", "Vanilluxe", "Deerling", "Sawsbuck", "Emolga", "Karrablast", "Escavalier", "Foongus", "Amoonguss", "Frillish", "Jellicent", "Alomomola", "Joltik", "Galvantula", "Ferroseed", "Ferrothorn", "Klink", "Klang", "Klinklang", "Tynamo", "Eelektrik", "Eelektross", "Elgyem", "Beheeyem", "Litwick", "Lampent", "Chandelure", "Axew", "Fraxure", "Haxorus", "Cubchoo", "Beartic", "Cryogonal", "Shelmet", "Accelgor", "Stunfisk", "Mienfoo", "Mienshao", "Druddigon", "Golett", "Golurk",
                   "Pawniard", "Bisharp", "Bouffalant", "Rufflet", "Braviary", "Vullaby", "Mandibuzz", "Heatmor", "Durant", "Deino", "Zweilous", "Hydreigon", "Larvesta", "Volcarona", "Cobalion", "Terrakion", "Virizion", "Tornadus", "Thundurus", "Reshiram", "Zekrom", "Landorus", "Kyurem", "Keldeo", "Meloetta", "Genesect", "Chespin", "Quilladin", "Chesnaught", "Fennekin", "Braixen", "Delphox", "Froakie", "Frogadier", "Greninja", "Bunnelby", "Diggersby", "Fletchling", "Fletchinder", "Talonflame", "Scatterbug",
                   "Spewpa", "Vivillon", "Litleo", "Pyroar", "Flabébé", "Floette", "Florges", "Skiddo", "Gogoat", "Pancham", "Pangoro", "Furfrou", "Espurr", "Meowstic", "Honedge", "Doublade", "Aegislash", "Spritzee", "Aromatisse", "Swirlix", "Slurpuff", "Inkay", "Malamar", "Binacle", "Barbaracle", "Skrelp", "Dragalge", "Clauncher", "Clawitzer", "Helioptile", "Heliolisk", "Tyrunt", "Tyrantrum", "Amaura", "Aurorus", "Hawlucha", "Dedenne", "Carbink", "Goomy", "Sliggoo", "Goodra", "Klefki", "Phantump", "Trevenant", "Pumpkaboo", "Gourgeist", "Bergmite", "Avalugg", "Noibat", "Noivern", "Xerneas", "Yveltal", "Zygarde", "Diancie", "Hoopa", "Volcanion", "Magearna"))
    Team <- c(LETTERS[1:min(teams,26)],
              letters[ifelse(teams>26,1,0):ifelse(teams>26,min(teams-26,26),0)],
              month.abb[ifelse(teams>52,1,0):ifelse(teams>52,min(teams-52,12),0)],
              month.name[ifelse(teams>64,1,0):ifelse(teams>64,min(teams-64,12),0)],
              pkmn[ifelse(teams>76,1,0):ifelse(teams>76,min(teams-76,722),0)])
  }
  if (is.null(strengths)) {
    Strength <- seq(2,.001,length.out=teams)
  } else {
    Strength <- strengths
  }
  if (swtch == FALSE) {
    Strength <- rev(sort(Strength))
  }
  TST <- data.frame(Team,Strength)
  divs <- matrix(Team,divisions,teams/divisions)
  tpd <- ncol(divs)
  gpw <- ((tpd-1)*divisions+odt*times)/weeks
  VM <- matrix(nrow=weeks,ncol=gpw*teams)
  Home <- vector()
  Visitor <- vector()
  ODM <- vector()
  for (j in 1:divisions) {
    for (i in 1:tpd) {
      opponents <- vector()
      if (i != tpd) {
        for (l in 1:gid) {
          opponents <- c(opponents,divs[j,(i+1):tpd])
        }
      }
      if (j != divisions) {
        for (l in 1:times) {
          aodt <- ifelse(divs[j,i] %in% ODM,odt-table(ODM)[names(table(ODM))==divs[j,i]],odt)
          newO <- sample(divs[-(1:j),],aodt)
          opponents <- c(opponents,newO)
          if (god.type == "teams") {
            ODM <- c(ODM,newO,rep(divs[j,i],length(newO)))
          }
        }
      }
      order <- sample(opponents)#,(tpd-i)*divisions+(odt-tpd*(j-1))*times)
      
      Home <- c(Home,rep(divs[j,i],length(order)))
      Visitor <- c(Visitor,order)
    }
  }
  Dat <- vector()
  L <- length(Home)/weeks
  for (i in (weeks-1):0) {
    Dat <- c(Dat,enddate-i*7-2)
  }
  Date <- c(rep(Dat,floor(L)),Dat[1:((L-floor(L))*weeks)])
  if (length(Home) > length(Date)) {
    Date <- c(Dat[(length(Dat)+1-abs(length(Home)-length(Date))):length(Dat)],Date)
  } else {
    if (length(Home) < length(Date)) {
      Date <- Date[1:(length(Date)-abs(length(Home)-length(Date)))]
    } 
  }
  HPTS <- vector()
  VPTS <- vector()
  for (i in 1:length(Home)) {
    if (type=="BT") {
      prb <- BTWP(TST$Strength[TST$Team==Home[i]],TST$Strength[TST$Team==Visitor[i]],FALSE)
      HPTS <- c(HPTS,sample(1:0,1,prob=c(prb,1-prb)))
      VPTS <- c(VPTS,ifelse(HPTS[i]==1,0,1))
    } else {
      prb <- MWP(TST$Strength[TST$Team==Home[i]],TST$Strength[TST$Team==Visitor[i]],FALSE)
      HPTS <- c(HPTS,sample(1:0,1,prob=c(prb,1-prb)))
      VPTS <- c(VPTS,ifelse(HPTS[i]==1,0,1))
    }
  }
  Winner <- ifelse(VPTS > HPTS,Visitor,Home)
  Loser <- ifelse(VPTS < HPTS,Visitor,Home)
  df <- data.frame(Date,Visitor,VPTS,Home,HPTS,Winner,Loser)
  df <- df[order(df$Date),]
  df$Date <- as.Date(df$Date,origin="1970-01-01")
  df$Day <- substr(weekdays(df$Date),1,3)
  `OT?` <- rep("NO",length(Home))
  df <- data.frame(Date=df$Date,Day=df$Day,Visitor=df$Visitor,VPTS=df$VPTS,Home=df$Home,HPTS=df$HPTS,`OT?`,Winner=df$Winner,Loser=df$Loser)
  df$Date <- as.Date(df$Date,origin="1970-01-01")
  df$Day <- as.character(df$Day)
  df$Home <- as.character(df$Home)
  df$Visitor <- as.character(df$Visitor)
  df$Winner <- as.character(df$Winner)
  df$Loser <- as.character(df$Loser)
  df$HPTS <- as.numeric(df$HPTS)
  df$VPTS <- as.numeric(df$VPTS)
  return(df)
}

progresspredict.simulation <- function(teams, weeks=NULL, strengths=NULL, drawtype=NULL, divisions,
                                       gid, god=NULL, god.type="times", enddate=Sys.Date(), type="BT",
                                       simulations=1000, arg1=NULL, arg2=NULL, arg3=NULL) {
  if (length(teams) > 1) {
    N <- length(teams)
  } else {
    N <- teams
  }
  strengthsTM <- rep(0, N)
  strengthsBT <- rep(0, N)
  wins <- rep(0, N)
  strongTM <- matrix(0, N, simulations)
  strongBT <- matrix(0, N, simulations)
  varsTM <- rep(0, N)
  varsBT <- rep(0, N)
  prcntTM <- vector()
  prcntBT <- vector()
  if (length(teams) > 1 && length(strengths) > 1) {
    swtch <- TRUE
  } else {
    swtch <- FALSE
  }
  if (length(teams) > 1) {
    Team <- teams
    teamsC <- length(teams)
    if (is.null(weeks)) {
      weeks <- teamsC
    }
  } else {
    if (is.null(weeks)) {
      weeks <- teams
    }
    teamsC <- teams
    pkmn <- sort(c("Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise", "Caterpie", "Metapod", "Butterfree", "Weedle", "Kakuna", "Beedrill", "Pidgey", "Pidgeotto", "Pidgeot", "Rattata", "Raticate", "Spearow", "Fearow", "Ekans", "Arbok", "Pichu", "Pikachu", "Raichu", "Sandshrew", "Sandslash", "NidoranM","NidoranF", "Nidorina", "Nidoqueen", "Nidorino", "Nidoking", "Cleffa", "Clefairy", "Clefable", "Vulpix", "Ninetales", "Igglybuff", "Jigglypuff", "Wigglytuff", "Zubat", "Golbat", "Crobat", "Oddish", "Gloom", "Vileplume", "Bellossom", "Paras", "Parasect", "Venonat", "Venomoth", "Diglett", "Dugtrio", "Meowth",
                   "Persian", "Psyduck", "Golduck", "Mankey", "Primeape", "Growlithe", "Arcanine", "Poliwag", "Poliwhirl", "Poliwrath", "Politoed", "Abra", "Kadabra", "Alakazam", "Machop", "Machoke", "Machamp", "Bellsprout", "Weepinbell", "Victreebel", "Tentacool", "Tentacruel", "Geodude", "Graveler", "Golem", "Ponyta", "Rapidash", "Slowpoke", "Slowbro", "Slowking", "Magnemite", "Magneton", "Magnezone", "Farfetchd", "Doduo", "Dodrio", "Seel", "Dewgong", "Grimer", "Muk", "Shellder", "Cloyster", "Gastly", "Haunter", "Gengar", "Onix", "Steelix", "Drowzee", "Hypno", "Krabby", "Kingler", "Voltorb", "Electrode", "Exeggcute", "Exeggutor", "Kangaskhan", "Cubone",
                   "Marowak", "Tyrogue", "Hitmonlee", "Hitmonchan", "Hitmontop", "Lickitung", "Lickilicky", "Koffing", "Weezing", "Rhyhorn", "Rhydon", "Rhyperior", "Happiny", "Chansey", "Blissey", "Tangela", "Tangrowth", "Horsea", "Seadra", "Kingdra", "Goldeen", "Seaking", "Staryu", "Starmie", "MimeJr", "MrMime", "Scyther", "Scizor", "Smoochum", "Jynx", "Elekid", "Electabuzz", "Electivire", "Magby", "Magmar", "Magmortar", "Pinsir", "Tauros", "Miltank", "Magikarp", "Gyarados", "Lapras", "Ditto", "Eevee", "Vaporeon", "Jolteon", "Flareon", "Espeon", "Umbreon", "Leafeon", "Glaceon", "Sylveon", "Porygon", "Porygon2", "PorygonZ", "Omanyte", "Omastar", "Kabuto",
                   "Kabutops", "Aerodactyl", "Munchlax", "Snorlax", "Articuno", "Zapdos", "Moltres", "Dratini", "Dragonair", "Dragonite", "Mewtwo", "Mew", "Chikorita", "Bayleef", "Meganium", "Cyndaquil", "Quilava", "Typhlosion", "Totodile", "Croconaw", "Feraligatr", "Sentret", "Furret", "Hoothoot", "Noctowl", "Ledyba", "Ledian", "Spinarak", "Ariados", "Chinchou", "Lanturn", "Togepi", "Togetic", "Togekiss", "Natu", "Xatu", "Mareep", "Flaaffy", "Ampharos", "Azurill", "Marill", "Azumarill", "Bonsly", "Sudowoodo", "Hoppip", "Skiploom", "Jumpluff", "Aipom", "Ambipom", "Sunkern", "Sunflora", "Yanma", "Yanmega", "Wooper", "Quagsire", "Murkrow", "Honchkrow",
                   "Misdreavus", "Mismagius", "Unown", "Wynaut", "Wobbuffet", "Girafarig", "Pineco", "Forretress", "Dunsparce", "Gligar", "Gliscor", "Snubbull", "Granbull", "Qwilfish", "Shuckle", "Heracross", "Sneasel", "Weavile", "Teddiursa", "Ursaring", "Slugma", "Magcargo", "Swinub", "Piloswine", "Mamoswine", "Corsola", "Remoraid", "Octillery", "Delibird", "Mantyke", "Mantine", "Skarmory", "Houndour", "Houndoom", "Phanpy", "Donphan", "Stantler", "Smeargle", "Raikou", "Entei", "Suicune", "Larvitar", "Pupitar", "Tyranitar", "Lugia", "HoOh", "Celebi", "Treecko", "Grovyle", "Sceptile", "Torchic", "Combusken", "Blaziken", "Mudkip", "Marshtomp", "Swampert",
                   "Poochyena", "Mightyena", "Zigzagoon", "Linoone", "Wurmple", "Silcoon", "Beautifly", "Cascoon", "Dustox", "Lotad", "Lombre", "Ludicolo", "Seedot", "Nuzleaf", "Shiftry", "Taillow", "Swellow", "Wingull", "Pelipper", "Ralts", "Kirlia", "Gardevoir", "Gallade", "Surskit", "Masquerain", "Shroomish", "Breloom", "Slakoth", "Vigoroth", "Slaking", "Nincada", "Ninjask", "Shedinja", "Whismur", "Loudred", "Exploud", "Makuhita", "Hariyama", "Nosepass", "Probopass", "Skitty", "Delcatty", "Sableye", "Mawile", "Aron", "Lairon", "Aggron", "Meditite", "Medicham", "Electrike", "Manectric", "Plusle", "Minun", "Volbeat", "Illumise", "Budew", "Roselia",
                   "Roserade", "Gulpin", "Swalot", "Carvanha", "Sharpedo", "Wailmer", "Wailord", "Numel", "Camerupt", "Torkoal", "Spoink", "Grumpig", "Spinda", "Trapinch", "Vibrava", "Flygon", "Cacnea", "Cacturne", "Swablu", "Altaria", "Zangoose", "Seviper", "Lunatone", "Solrock", "Barboach", "Whiscash", "Corphish", "Crawdaunt", "Baltoy", "Claydol", "Lileep", "Cradily", "Anorith", "Armaldo", "Feebas", "Milotic", "Castform", "Kecleon", "Shuppet", "Banette", "Duskull", "Dusclops", "Dusknoir", "Tropius", "Chingling", "Chimecho", "Absol", "Snorunt", "Glalie", "Froslass", "Spheal", "Sealeo", "Walrein", "Clamperl", "Huntail", "Gorebyss", "Relicanth", "Luvdisc",
                   "Bagon", "Shelgon", "Salamence", "Beldum", "Metang", "Metagross", "Regirock", "Regice", "Registeel", "Latias", "Latios", "Kyogre", "Groudon", "Rayquaza", "Jirachi", "Deoxys", "Turtwig", "Grotle", "Torterra", "Chimchar", "Monferno", "Infernape", "Piplup", "Prinplup", "Empoleon", "Starly", "Staravia", "Staraptor", "Bidoof", "Bibarel", "Kricketot", "Kricketune", "Shinx", "Luxio", "Luxray", "Cranidos", "Rampardos", "Shieldon", "Bastiodon", "Burmy", "Wormadam", "Mothim", "Combee", "Vespiquen", "Pachirisu", "Buizel", "Floatzel", "Cherubi", "Cherrim", "Shellos", "Gastrodon", "Drifloon", "Drifblim", "Buneary", "Lopunny", "Glameow", "Purugly",
                   "Stunky", "Skuntank", "Bronzor", "Bronzong", "Chatot", "Spiritomb", "Gible", "Gabite", "Garchomp", "Riolu", "Lucario", "Hippopotas", "Hippowdon", "Skorupi", "Drapion", "Croagunk", "Toxicroak", "Carnivine", "Finneon", "Lumineon", "Snover", "Abomasnow", "Rotom", "Uxie", "Mesprit", "Azelf", "Dialga", "Palkia", "Heatran", "Regigigas", "Giratina", "Cresselia", "Phione", "Manaphy", "Darkrai", "Shaymin", "Arceus", "Victini", "Snivy", "Servine", "Serperior", "Tepig", "Pignite", "Emboar", "Oshawott", "Dewott", "Samurott", "Patrat", "Watchog", "Lillipup", "Herdier", "Stoutland", "Purrloin", "Liepard", "Pansage", "Simisage", "Pansear", "Simisear",
                   "Panpour", "Simipour", "Munna", "Musharna", "Pidove", "Tranquill", "Unfezant", "Blitzle", "Zebstrika", "Roggenrola", "Boldore", "Gigalith", "Woobat", "Swoobat", "Drilbur", "Excadrill", "Audino", "Timburr", "Gurdurr", "Conkeldurr", "Tympole", "Palpitoad", "Seismitoad", "Throh", "Sawk", "Sewaddle", "Swadloon", "Leavanny", "Venipede", "Whirlipede", "Scolipede", "Cottonee", "Whimsicott", "Petilil", "Lilligant", "Basculin", "Sandile", "Krokorok", "Krookodile", "Darumaka", "Darmanitan", "Maractus", "Dwebble", "Crustle", "Scraggy", "Scrafty", "Sigilyph", "Yamask", "Cofagrigus", "Tirtouga", "Carracosta", "Archen", "Archeops", "Trubbish",
                   "Garbodor", "Zorua", "Zoroark", "Minccino", "Cinccino", "Gothita", "Gothorita", "Gothitelle", "Solosis", "Duosion", "Reuniclus", "Ducklett", "Swanna", "Vanillite", "Vanillish", "Vanilluxe", "Deerling", "Sawsbuck", "Emolga", "Karrablast", "Escavalier", "Foongus", "Amoonguss", "Frillish", "Jellicent", "Alomomola", "Joltik", "Galvantula", "Ferroseed", "Ferrothorn", "Klink", "Klang", "Klinklang", "Tynamo", "Eelektrik", "Eelektross", "Elgyem", "Beheeyem", "Litwick", "Lampent", "Chandelure", "Axew", "Fraxure", "Haxorus", "Cubchoo", "Beartic", "Cryogonal", "Shelmet", "Accelgor", "Stunfisk", "Mienfoo", "Mienshao", "Druddigon", "Golett", "Golurk",
                   "Pawniard", "Bisharp", "Bouffalant", "Rufflet", "Braviary", "Vullaby", "Mandibuzz", "Heatmor", "Durant", "Deino", "Zweilous", "Hydreigon", "Larvesta", "Volcarona", "Cobalion", "Terrakion", "Virizion", "Tornadus", "Thundurus", "Reshiram", "Zekrom", "Landorus", "Kyurem", "Keldeo", "Meloetta", "Genesect", "Chespin", "Quilladin", "Chesnaught", "Fennekin", "Braixen", "Delphox", "Froakie", "Frogadier", "Greninja", "Bunnelby", "Diggersby", "Fletchling", "Fletchinder", "Talonflame", "Scatterbug",
                   "Spewpa", "Vivillon", "Litleo", "Pyroar", "Flabébé", "Floette", "Florges", "Skiddo", "Gogoat", "Pancham", "Pangoro", "Furfrou", "Espurr", "Meowstic", "Honedge", "Doublade", "Aegislash", "Spritzee", "Aromatisse", "Swirlix", "Slurpuff", "Inkay", "Malamar", "Binacle", "Barbaracle", "Skrelp", "Dragalge", "Clauncher", "Clawitzer", "Helioptile", "Heliolisk", "Tyrunt", "Tyrantrum", "Amaura", "Aurorus", "Hawlucha", "Dedenne", "Carbink", "Goomy", "Sliggoo", "Goodra", "Klefki", "Phantump", "Trevenant", "Pumpkaboo", "Gourgeist", "Bergmite", "Avalugg", "Noibat", "Noivern", "Xerneas", "Yveltal", "Zygarde", "Diancie", "Hoopa", "Volcanion", "Magearna"))
    Team <- c(LETTERS[1:min(teams,26)],
              letters[ifelse(teams>26,1,0):ifelse(teams>26,min(teams-26,26),0)],
              month.abb[ifelse(teams>52,1,0):ifelse(teams>52,min(teams-52,12),0)],
              month.name[ifelse(teams>64,1,0):ifelse(teams>64,min(teams-64,12),0)],
              pkmn[ifelse(teams>76,1,0):ifelse(teams>76,min(teams-76,722),0)])
  }
  if (is.null(strengths)) {
    if (is.null(drawtype)) {
      Strength <- seq(2,.001,length.out=teamsC)
    } else {
      if (is.null(arg1)) {
        Strength <- (abs(drawtype(teamsC))+.001)
      } else {
        if (is.null(arg2)) {
          Strength <- (abs(drawtype(teamsC,arg1))+.001)
        } else {
          if (is.null(arg3)) {
            Strength <- (abs(drawtype(teamsC,arg1,arg2))+.001)
          } else {
            Strength <- (abs(drawtype(teamsC,arg1,arg2,arg3))+.001)
          }
        }
      }
    }
  } else {
    Strength <- strengths
  }
  if (swtch == FALSE) {
    Strength <- rev(sort(Strength))
  }
  
  strengths <- Strength
  cmprsn <- table(c("Bradley-Terry","Thurstone-Mosteller","Tie"))-1
  fllcmp <- table(c("Bradley-Terry","Thurstone-Mosteller","Tie"))-1
  
  TST <- data.frame(Team,Strength)
  for (i in 1:simulations) {
    if (is.null(drawtype)) {
      df <- full.sim.gen(teams, weeks, strengths, divisions, gid, god, god.type, enddate, type)
    } else {
      if (is.null(arg1)) {
        df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC))+.001), divisions, gid, god,
                           god.type, enddate, type)
      } else {
        if (is.null(arg2)) {
          df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC,arg1))+.001), divisions, gid, 
                             god, god.type, enddate, type)
        } else {
          if (is.null(arg3)) {
            df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC,arg1,arg2))+.001), divisions, gid, 
                               god, god.type, enddate, type)
          } else {
            df <- full.sim.gen(teams, weeks, (abs(drawtype(teamsC,arg1,arg2,arg3))+.001), divisions,
                               gid, god, god.type, enddate, type)
          }
        }
      }
    }
    
    round <- dataconfigure(df)
    
    pp <- progresspredict(df)
    
    cmprsn <- cmprsn + pp$Comparison
    if(pp$Comparison[1]==pp$Comparison[2]){
      fllcmp <- fllcmp + c(0,0,1)
    } else {
      fllcmp <- fllcmp + ifelse(pp$Comparison==max(pp$Comparison),1,0)
    }
    
    pt <- pp$ProgressionTable
    if (i == 1) {
      plot(pt$Week,pt$ThurstoneMosteller,col=2,ylim=c(0,teams))
    } else {
      points(pt$Week,pt$ThurstoneMosteller,pch=i,col=2)
    }
    points(pt$Week,pt$BradleyTerry,pch=i,col=4,cex=.75)
    
    samdf <- data.frame(Team=round$Team,WT=round$WinsTotal,MS=pp$FinalTMStrenghs,BTS=pp$FinalBTStrenghs)
    
    prpTM <- table(c(TST[order(-TST$Strength),]$Team,1,0)==c(samdf[order(-samdf$TMS),]$Team,0,1))
    prpTM <- prpTM-1
    prcntTM <- c(prcntTM,prop.table(prpTM)[2])
    prpBT <- table(c(TST[order(-TST$Strength),]$Team,1,0)==c(samdf[order(-samdf$BTS),]$Team,0,1))
    prpBT <- prpBT-1
    prcntBT <- c(prcntBT,prop.table(prpBT)[2])
    
    strongTM[,i] <- pp$FinalTMStrenghs
    strongBT[,i] <- pp$FinalBTStrenghs
    strengthsTM <- ((i - 1) * strengthsTM + pp$FinalTMStrenghs)/i
    strengthsBT <- ((i - 1) * strengthsBT + pp$FinalBTStrenghs)/i
    wins <- ((i - 1) * wins + round$WinsTotal)/i
  }
  varsTM <- vector()
  varsBT <- vector()
  for (i in 1:N) {
    varsTM[i] <- var(strongTM[i,])
    varsBT[i] <- var(strongBT[i,])
  }
  biTM <- strengthsTM/mean(strengthsTM) - TST$Strength/mean(TST$Strength)
  biBT <- strengthsBT/mean(strengthsBT) - TST$Strength/mean(TST$Strength)
  newdf <- data.frame(TST$Team, strengthsBT, strengthsTM, wins)
  names(newdf) <- c("Team","BradleyTerrySimulatedStrength","ThurstoneMostellerSimulatedStrength",
                    "WinsSummary")
  prcntgs <- prop.table(cmprsn)
  
  return(list(SimulatedData=newdf,BradleyTerryStrengths=strongBT,ThurstoneMostellerStrengths=strongM,
              BradleyTerryVariance=varsBT,ThurstoneMostellerVariance=varsM,BradleyTerryBias=biBT,
              ThurstoneMostellerBias=biM,BradleyTerryPercentPerfect=prcntBT,ThurstoneMostellerPercentPerfect=prcntM,
              FullSimulationComparison=fllcmp,Comparison=cmprsn,Proportion=prcntgs))
}
