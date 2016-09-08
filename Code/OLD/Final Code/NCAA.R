teams <- read.csv("book1.csv",FALSE)
t <- gsub(" ", "", teams$V1, fixed = TRUE)
head(teams)
NCAAScores$Home <- casefold(NCAAScores$Home,TRUE)
NCAAScores$Visitor <- casefold(NCAAScores$Visitor,TRUE)
go <- proc.time()
reldate <- Sys.Date()
forsim <- FALSE
  tt <- length(unique(t))
  
  if (forsim == TRUE) {
    reldate <- Sys.Date() + 365
  }
  
  #these first lines create a new dataframe with Team, Strength, and Wins
  data2 <- data.frame(sort(unique(t)),rep(1,tt),rep(1,tt))
                     #table(c(NCAAScores$Winner[NCAAScores$Date <= reldate-1],unique(NCAAScores$Loser)))-1)
  names(data2) <- c("Team","Strength","WinsTotal")
  data$Temp <- NULL
  #the rest of the lines within the function create the versus matrix by first creating
  # an empty ttxtt matrix and then filling it via a for loops.
  mm <- matrix(0, tt, tt)
  ww <- matrix(0, tt, tt)
  pstn <- 0
  for (i in data2$Team){
    for (n in data2$Team){
      pstn <- pstn + 1
      x <- 0
      z <- 0
      for (y in 1:length(which(NCAAScores$Date <= Sys.Date()-1))){
        if(NCAAScores$Home[y]==i && NCAAScores$Visitor[y]==n){
          x <- x + 1
          if(NCAAScores$HPTS[y] < NCAAScores$VPTS[y]){
            z <- z + 1
          }
        }
        if(NCAAScores$Home[y]==n && NCAAScores$Visitor[y]==i){
          x <- x + 1
          if(NCAAScores$HPTS[y] > NCAAScores$VPTS[y]){
            z <- z + 1
          }
        }
      }
      mm[pstn] <- x
      ww[pstn] <- z
    }
  }
  data2$Versus <- mm
  data2$WinsVersus <- ww
  data2$Team <- as.character(data$Team)
  
  if (forsim == TRUE) {
    data$WinsVersus <- NULL
    data$WinsTotal <- NULL
  }
  View(data2)
total <- proc.time() - go
total

bracket2 <- LARC.Rank(data2)
View(bracket2)

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
head(Scoresdf)
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

go <- proc.time()
NCAAdf <- dataconfigure(Scoresdf)
total <- proc.time() - go
total
Sys.time()
brack <- LARC.Rank(NCAAdf)
View(brack)
