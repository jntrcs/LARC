#The following function takes the dataframe of the NBA data and converts it into a format
# usable for running optimization. The new dataframe includes a "Team" variable that has 
# each NBA team listed once, as such this dataframe only has 30 observations (one for each
# team). The next variable is "Strength" and is one for every team when this function is
# used. "Wins" is the third variable and is the number of wins that team has had. Finally,
# the variable "Versus" is a matrix that includes the number of times each team plays each
# opponent, where the first column of the matrix is associated with the first observation
# or the first team listed in the dataframe. It can also take a reldate (relevant date)
# arguement if you only wish to include data from before a certain date, the date must be 
# in the following format "YYYY-MM-DD". There is also a third arguement forsim that is set
# to FALSE as the default but should be set to TRUE to create data frame without the Wins
# variables and including a complete Versus matrix for all the games to be included within
# that sport season.
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
                     table(c(df$Winner[df$Date <= reldate],unique(df$Loser)))-1)
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

#These lines of code show the funtion in action. The commented out lines time how long it 
# takes to run the code and display the resulting dataframe.
#go <- proc.time()
NBAdf <- dataconfigure(NBAScores)
#NBAdf
#proc.time() - go
NHLdf <- dataconfigure(NHLScores)
#NFLdf <- dataconfigure(NFLScores)

#The remainder of the code demonstrates the "Versus" matrix and how to get info from it.

#to call the entire matrix input NBAdf$Versus (replacing NBAdf with the name of your df
# as should be done with all of the following examples)
NBAdf$Versus

#to call only one column input NBAdf$Versus[,(column #)]
NBAdf$Versus[,1]
#to call only one row input NBAdf$Versus[(row #),]
NBAdf$Versus[1,]
#it is worth noting that rows and columns of the same number are identical

#there are two ways too call a specific value from the matrix:
# the first way is to input NBAdf$Versus[(row #),(column #)] which displays the amount of 
# times the teams of those two numbers have played each other. Entering the same number 
# twice will always return 0 as a team cannot play itself.
NBAdf$Versus[4,2]
NBAdf$Versus[22,30]

# the second way is to input NBAdf$Versus[#] which displays the amount of games denoted in
# that position of the matrix starting in the top-left corner going down each column until
# reaching the bottom-right corner, including all positive integers below or equal to 900
NBAdf$Versus[34] # or the number of times the second team has played the fourth team
NBAdf$Versus[892] # or the number of times the thirtieth team has played the twenty-second
