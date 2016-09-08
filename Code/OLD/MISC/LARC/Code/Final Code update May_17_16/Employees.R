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
EmployeeRank <- LARC.Rank(Employeedf)
EmployeeRank <- LARC.Rank(Employeedf,BradleyTerryLARC)
EmployeeRank <- LARC.Rank(Employeedf,MostellerLARC)
