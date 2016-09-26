#This is a function to automate the datascrape of the NBA data. It could be adjusted to
# accomodate other types of data as well, but for now it will only scrape for NBA data.
#If the website down you use try this http://webcache.googleusercontent.com/search?q=cache:http://www.basketball-reference.com/leagues/NBA_2016_games.html
#original website http://www.basketball-reference.com/leagues/NBA_
#install.packages("XML")

#All specific sports are called within Datascrape(). Therefore any outside callers should call datascrape("Sport") and not the sport directly

scrapeNBA<-function(playoffs=FALSE,year=substr(Sys.Date(),1,4))
{
  url <- paste("http://webcache.googleusercontent.com/search?q=cache:http://www.basketball-reference.com/leagues/NBA_",year,"_games.html",sep="")
  tables <- readHTMLTable(url)
  if (playoffs==TRUE) {
    tables[[1]] <- rbind(tables[[1]],tables[[2]])
  }
  names(tables[[1]]) <- c("Date","Start(ET)","Visitor","VPTS","Home",
                          "HPTS","Boxscore","OT?","Notes")
  tables[[1]]$Day <- substr(tables[[1]]$Date,1,3)
  tables[[1]]$Date <- as.Date(gsub(",","",substr(tables[[1]]$Date,6,
                                                 nchar(as.character(tables[[1]]$Date)))), "%b %d %Y")
  tables[[1]]
}

scrapeNHL<-function(playoffs=FALSE,year=substr(Sys.Date(),1,4))
{
  url <- paste("http://www.hockey-reference.com/leagues/NHL_",year,"_games.html",sep="")
  tables <- readHTMLTable(url)
  if (playoffs==TRUE) {
    tables[[1]] <- rbind(tables[[1]],tables[[2]])
  }
  names(tables[[1]]) <- c("Date","Visitor","VPTS","Home","HPTS","OT?"," ","Attendance",
                          "LOG","Notes1","Notes2")
  tables[[1]]$Date <- as.Date(tables[[1]]$Date, "%Y-%m-%d")
  tables[[1]]$Day <- substr(weekdays(tables[[1]]$Date),1,3)
  table[[1]]
  
}

scrapeNFL<-function(playoffs=FALSE,year=substr(Sys.Date(),1,4)){
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
  tables[[1]]
}

scrapeNCAAB<-function(playoffs=FALSE,year=substr(Sys.Date(),1,4)){
  tables <- read.fwf(file=url("http://masseyratings.com/scores.php?s=284067&sub=11590&all=1&mode=3&sch=on&format=0"),
                     skip=39, n=5869, widths=c(10, 2, 24, 3, 2, 24, 3, 10, 25),
                     col.names=c("Date","Where1","Team1","PTS1","Where2","Team2","PTS2","OT?","Notes"))
  
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
  tables[[1]]
}

scrapeNCAAF<-function()
{
  #Am skipping the first game because of formatting issues, add it in manually?
  table1 <- read.fwf(file=url("http://www.masseyratings.com/scores.php?s=286577&sub=11604&all=1"),
                     skip=25, n=-1, widths=c(10, 2, 24, 3, 2, 24, 3, 4, 25),
                     col.names=c("Date","Where1","Team1","PTS1","Where2","Team2","PTS2","OT?","Notes"),
                     strip.white=TRUE)
  table1<-table1[1:(nrow(table1)-6),]
  
  table1$PTS1 <- as.character(table1$PTS1)
  table1$PTS2 <- as.character(table1$PTS2)
  table1$Date <- gsub("<hr><pre>","",as.character(table1$Date))
  table1$Date<-as.Date(table1$Date)
  

  table1$Visitor <- ifelse(table1$Where1 == " @",as.character(table1$Team2),
                                as.character(table1$Team1))
  table1$Home <- ifelse(table1$Where1 == " @",as.character(table1$Team1),
                             as.character(table1$Team2))
  table1$VPTS <- ifelse(table1$Where1 == " @",as.character(table1$PTS2),
                             as.character(table1$PTS1))
  table1$HPTS <- ifelse(table1$Where1 == " @",as.character(table1$PTS1),
                             as.character(table1$PTS2))
  table1$Day<- substr(weekdays(table1$Date),1,3)
  for (i in 1:nrow(table1)) {
    if (length(grep("O",table1$`OT?`[i]))==1) {
      table1$`OT?`[i] <- table1$`OT?`[i]
    } else {
      table1$`OT?`[i] <- "NO"
    }
  }
  
a<-data.frame("2016-08-26", " ", "Hawaii", 31, " ", "California", 51, " ", "Australia", "Hawaii", "California", 31, 51, "Fri", "NO")
names(a)<-names(table1)
rbind(table1, a)
}

datascrape <- function(datatype,playoffs=FALSE,year=substr(Sys.Date(),1,4), date = Sys.Date()){
  
  #this package is needed in order to scrape the data off the internet.
  library(XML)
  
  #the following lines of code scrape the data off of the website that has the URL 
  # corresponding with the chosen data type
  if(datatype=="NBA"){
   table1<-scrapeNBA()
  }
  
  else if(datatype=="NHL"){
    table1<-scrapeNHL()
  }
  
  else if(datatype=="NFL") {
    table1<-scrapeNFL()
  }
  
  else if(datatype=="NCAAB"){
    table1<-scrapeNCAAB()
  }
  
  else if (datatype=="NCAAF")
  {
    table1<-scrapeNCAAF()
  }
  
  #the next few lines create a dataframe with the data that we care about, removing empty
  # and irrelevant columns
  Scoresdf <- data.frame(table1$Date,table1$Day,table1$Visitor,
                         table1$VPTS,table1$Home,table1$HPTS,
                         table1$`OT?`)
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
  Scoresdf
}


###TODO: fix parameters to require what is needed and respect choices. 
#Fix NFL and NHL (I don't think I broke those)
#Get rid of NCAAF warning message
