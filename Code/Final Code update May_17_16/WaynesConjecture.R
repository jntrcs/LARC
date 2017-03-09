#Illustrates a difference between the two methods: when a team is ranked by BT and they have played the same 
#teams and have the same number of wins, their strengths will always be the same.
#With TM, who they beat will influence their ranking.
winsMatrix<-matrix(c(0,1,1,1,1,1,1,1,1,0,
                     0,0,1,1,1,1,1,1,0,0,
                     0,0,0,1,1,1,1,1,0,0,
                     0,0,0,0,1,1,1,1,0,0,
                     0,0,0,0,0,1,1,1,0,0,
                     0,0,0,0,0,0,1,1,0,0,
                     0,0,0,0,0,0,0,1,0,0,
                     0,0,0,0,0,0,0,0,0,1,
                     0,1,1,1,1,1,1,1,0,0,
                     1,1,1,1,1,1,1,0,0,0), byrow = TRUE, ncol=10, nrow=10
                     )




df<-data.frame(Team=c(paste("T", 1:8, sep=""), "L1", "L8"), Strength=rep(1,10), WinsTotal = rowSums(winsMatrix))
df$WinsVersus<-winsMatrix
df
#Bradley-Terry:
LARC.Rank(df) 
#Thurstone-Mosteller
LARC.Rank(df, func=TMDensity)

