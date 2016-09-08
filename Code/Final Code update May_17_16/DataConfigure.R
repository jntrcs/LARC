# function takes a data frame, df and produces a new data frame. 
# Arguements Defined
# df -- the old data frame -- 9 variables defined
#    Game Number -- Int -- 1, 2, 3, ..., number of games in the data base
#    Team Names  -- Factor w/t levels (t is the number of teams) -- names of the teams followed by a team number 
#    Winner -- chr -- the winning team for each of the games
#    HPTS   -- number -- TBL (To be Defined Later)
#    Loser  -- chr -- the losing team for each of the games
# the old data frame varies by invoking program -- NBA data, employee data, etc.
# The resulting data frame is usuable for the optimazation of the posterior distribution
# The description of the new data frame is below
#   Data frame has t (number of teams) observations of 5 variables
# 
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
  #These commented out lines may need to be readded, I cannot figure out the purpose they serve
# but they were causing issues with some datasets and I don't think omitting them causes any issues
  } #else {
    #LD <- length(which(df$Date <= reldate))
    #while (is.na(df$HPTS[LD+1])) {
    #  reldate <- reldate - 1
    #  LD <- length(which(df$Date <= reldate))
    #}
  #}
#  
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

#These line of code shows the funtion in action.
# NBAdf <- dataconfigure(NBAScores)
