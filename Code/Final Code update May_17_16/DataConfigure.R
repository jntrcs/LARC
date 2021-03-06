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


dataconfigure <- function(df, reldate=Sys.Date()-1, forsim=FALSE) {
  if (class(reldate)=="Date") 
    df$Date <-as.Date(df$Date)
  df<-df[df$Date<=reldate,]
  tt <- length(unique(c(df$Home,df$Visitor)))


  #these first lines create a new dataframe with Team, Strength, and Wins
  data <- data.frame(sort(unique(c(df$Home,df$Visitor))),rep(1,tt),
                     table(c(df$Winner,unique(c(df$Home,df$Visitor))))-1)
  names(data) <- c("Team","Strength","Temp","WinsTotal")
  data$Temp <- NULL
  #the rest of the lines within the function create the versus matrix by first creating
  # an empty ttxtt matrix and then filling it via a for loop.
  ww <- matrix(0, tt, tt)

  for (i in 1:nrow(df))
  {
    indexTeam1<-which(df$Home[i]==data$Team, arr.ind = TRUE)
    indexTeam2<-which(df$Visitor[i]==data$Team, arr.ind = TRUE)
    team1Won<-df$Home[i]==df$Winner[i]
 
    if (team1Won)
    {
      ww[indexTeam1,indexTeam2]<-ww[indexTeam1,indexTeam2]+1
    }
    else{
      ww[indexTeam2,indexTeam1]<-ww[indexTeam2,indexTeam1]+1
    }
  }
    #data$Versus <- mm
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

