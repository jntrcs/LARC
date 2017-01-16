##Analyze Excel sheet


#Remember, this doesn't handle ties.
#Probability that team A beats team B = strength(A)/(strength(A)/Strength(B))
#When you specify the file path make sure to use double back slash instead of single back slash
#Save the template as a .csv, not a .xls file

load("MasterFunctionFile.RData")
Rcpp::sourceCpp("cppFiles.cpp")
analyzeExcel(path="C:\\Users\\jntrcs\\Desktop\\Apex\\Template.csv")




analyzeExcel<-function(path)
{
 dat<-read.csv(path) 
 dat$HomeTeam<-as.character(dat$HomeTeam)
 dat$AwayTeam<-as.character(dat$AwayTeam)
 dat$Date<-rep(Sys.Date()-2, nrow(dat))
 dat$Winner<-ifelse(dat$HomeScore>dat$AwayScore, dat$HomeTeam, dat$AwayTeam)
 dat$Loser<-ifelse(dat$HomeScore<dat$AwayScore, dat$HomeTeam, dat$AwayTeam)
 names(dat)<-c("Home", "HomeScore", "Visitor", "VisitorScore", "Date", "Winner", "Loser")
 con<-dataconfigure(dat)
 LARC.Rank(con)
}
