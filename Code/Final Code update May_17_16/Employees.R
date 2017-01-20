# Employees is the 'Main' program for computing employee strengths based on 48 pairwise comparisons made by 12 ficitional employees
# These fictional employees are called Judges in this code
# Note -- as it stands now the code never uses the information about judges.  That is for future research
# Each of the 48 comparisons have a 'judge,' a 'winner,' and a 'loser'
# There are two data frames in the code described below
# 
# EmployeeScores -- 48 observations of 9 variables
#    Comparision -- the integers between 1 and 48
#    Judge --  the names of the 12 judges
#    Winner -- the names of the 48 winners
#    HTPS -- whether the winner was on his home court
#            note homecourt has not been used in any of our calculations.  It was put in these data frames for future research
#            for this data all the home courts are listed as '1'
#    Loser  -- the names of the 48 losers
#    VTPS   -- Which 'team' was on the vistors court -- again not used -- for this data all are set to 0
#    Date   -- Date is used in the calculations for NBA and such, but not used here so some arbitary data is specified
#    Home   -- For the employee data each was presented two names for which the judge was to pick a winner.  The first name is considered as home.
#    Visitor - The second name presented to the judge.  Again all of this is for future research.
#  
# The code takes the above data frame and involves the function 'dataconfigure' to produce the second dataframe
# Employeedf -- 12 observations of 5 variables
# Team -- the names of the 12 employees
# Strength -- the employees strength -- for the Bradley-Terry Model these are initially 1.0
#                                    -- for the Mosteller Model these are initially 0.0
#                                    -- at the end of this program these are the estimated strengths based on the data
# WinsTotal -- the number of wins each for each 'team' 
# Versus    -- a 12 by 12 matrix
#           -- the number of times each team 'plays' each other team 
# WinVersus -- a 12 bt 12 matrix
#           -- the number of times each team beats each other team
#
# Employees invokes two other functions, which in turn invokes lower leverl functions
#  dataconfigure
#  Larc.Rank
#
Comparison <- 1:48
Judge <- rep(c("Bob","Jim","Kara","Sally","Al","Rich","Bill","Ellen",
               "Andrew","Ross","Mary","Jane"),4)
Winner <- c("Jim","Rich","Mary","Mary","Ellen","Sally","Sally","Al","Bill","Al","Jim",
            "Sally","Bob","Ellen","Jane","Mary","Ellen","Jim","Jane","Kara","Ross","Ellen",
            "Ellen","Sally","Sally","Mary","Jane","Mary","Jim","Mary","Sally","Bob","Bob",
            "Bill","Ellen","Ellen","Jim","Jane","Bob","Mary","Kara","Bill","Bill","Andrew",
            "Jim","Jim","Rich","Andrew")
Loser <- c("Kara","Ellen","Andrew","Al","Kara","Rich","Mary","Kara","Al","Jane","Andrew",
           "Bob","Jim","Bill","Al","Al","Kara","Bob","Bill","Rich","Andrew","Andrew",
           "Sally","Bob","Rich","Rich","Ross","Ross","Al","Jane","Ross","Andrew","Rich",
           "Al","Jane","Bob","Mary","Rich","Bill","Ellen","Andrew","Bob","Sally","Ross",
           "Kara","Ross","Jim","Ross")

EmployeeScores <- data.frame(Comparison,Judge,Winner,HPTS = 1,Loser,VPTS = 0,
                             Date = as.Date(Sys.Date()-2))
EmployeeScores$Winner <- as.character(EmployeeScores$Winner)
EmployeeScores$Loser <- as.character(EmployeeScores$Loser)
EmployeeScores$Home <- as.character(EmployeeScores$Winner)
EmployeeScores$Visitor <- as.character(EmployeeScores$Loser)
Employeedf <- dataconfigure(EmployeeScores)
EmployeeRank<- LARC.Rank(Employeedf)
EmployeeRank2<-LARC.Rank(Employeedf, func=BradleyTerryLARC)
cbind(EmployeeRank, EmployeeRank2)
