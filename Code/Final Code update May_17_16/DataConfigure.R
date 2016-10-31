# Last comments entered 2016-09-06 WAL
# function takes a data frame, df and produces a new data frame. 
# Three arguements Defined
# df -- the old data frame -- 9 variables defined
#    Game Number -- Int -- 1, 2, 3, ..., number of games in the data base
#    Team Names  -- Factor w/t levels (t is the number of teams) -- names of the teams followed by a team number 
#    Winner -- chr -- the winning team for each of the games
#    HPTS   -- num -- Home Points
#    Loser  -- chr -- the losing team for each of the games
#    VPTS   -- num -- Visitor Points
#    Date   -- Date-- One can limit the data for NBA, NCAA football, etc. only up to a certain point 
#    Home   -- chr -- name of the winning team
#    Visitor-- chr -- name of the losing team 
# reldate -- a date variable, the default is Sys.Date() - 1
# forsim  -- logical -- default = FALSE -- can be set to True to create the new data frame without the WinsVersus matrix
# the old data frame varies by invoking program -- NBA data, employee data, etc.
# The resulting new data frame is usuable for the optimazation of the posterior distribution
# The description of the new data frame is below
#   Data frame has t (number of teams) observations of 5 variables
# Team --       chr -- name of the teams
# Strength --   num -- initial strength for Bradly-Terry (BT)is 1,1,1... 
#                      initial strength for Thurstone-Mosteller (TM)is 0,0,0,...
# WinsTotal --  num -- number of wins for each team
# versus--      num -- t x t matrix of the number of times each team plays each team
# WinsVersus -- num -- t x t matrix of the number of times each team beats each team
#
#
df<-raw
reldate<-"2016-09-18"
dataconfigure <- function(df, reldate=Sys.Date()-1, forsim=FALSE) {
  df$Date <-as.Date(df$Date)
  df<-df[df$Date<=reldate,]
  tt <- length(unique(c(df$Home,df$Visitor)))
  
  if (forsim == TRUE) {
    reldate <- Sys.Date() + 365
# All future comments were added by Ala'a    
# These commented out lines may need to be readded, I cannot figure out the purpose they serve
# but they were causing issues with some datasets and I don't think omitting them causes any issues
} 
# else {
# LD <- length(which(df$Date <= reldate))
# while (is.na(df$HPTS[LD+1])) {
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

#This function accepts the data.frame with the versus matrix and a data.frame with bradley terry strengths
#and with Thurstone mosteller strengths, and attaches them to the matrix DF
attachMostRecentStrengths<-function(matrix, BTstrengths, TMStrengths)
{

   for (j in 1:length(matrix$Team))
  {
      matrix$BTLast[j]<-ifelse(matrix$Team[j] %in% BTstrengths$Team,
                     BTstrengths$Strength[BTstrengths$Team ==matrix$Team[j]],
                                                     1)
   matrix$TMLast[j]<-ifelse(matrix$Team[j] %in% TMStrengths$Team,
                                        TMStrengths$Strength[TMStrengths$Team ==matrix$Team[j]],
                                          0)
   }
  matrix
}

#for (i in 12:17)
#all2015data[[i]][[1]]<-attachMostRecentStrengths(all2015data[[i]][[1]], all2015data[[11]][[2]], all2015data[[11]][[3]])
