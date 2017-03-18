##Analyze Excel sheet


#To use, change the path and then run the first three lines (highlight and hit run, control-r) (ignore the stuff below)
#Probability that team A beats team B = strength(A)/(strength(A)/Strength(B))
#When you specify the file path make sure to use double back slash instead of single back slash
#Save the template as a .csv, not a .xls file

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")
##Defaults to Bradley-Terry, set useTM to TRUE for Thurstone-Mosteller
analyzeExcel(path="Template.csv", useTM=F)




analyzeExcel<-function(path, useTM=FALSE)
{
 dat<-read.csv(path) 
 names(dat)<-c("HomeTeam", "HomeScore", "AwayTeam", "AwayScore") 
 
 dat$HomeTeam<-as.character(dat$HomeTeam)
 dat$AwayTeam<-as.character(dat$AwayTeam)
 dat$Date<-rep(Sys.Date()-2, nrow(dat))
 dat$Winner<-ifelse(dat$HomeScore>dat$AwayScore, dat$HomeTeam, dat$AwayTeam)
 dat$Loser<-ifelse(dat$HomeScore<=dat$AwayScore, dat$HomeTeam, dat$AwayTeam)
 names(dat)<-c("Home", "HomeScore", "Visitor", "VisitorScore", "Date", "Winner", "Loser")
 con<-dataconfigure(dat)
 func<-ifelse(useTM, TMDensity, BTDensity)
 LARC.Rank(con, func=func)
}

analyzeExcel("brawlData.txt")
b=newMetHast(logBTDensity, 10000, con$WinsVersus, 1.4)

