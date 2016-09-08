
vs <- matrix(c(0,1,
               1,0),2,2)
vw <- matrix(c(0,0,
               1,0),2,2)
Sample1 <- data.frame(1:2,c(1,0),1)
names(Sample1) <- c("Team","WinsTotal","Strength")
Sample1$Versus <- vs
Sample1$WinsVersus <- vw
Sample1

vs <- matrix(c(0,2,
               2,0),2,2)
vw <- matrix(c(0,0,
               2,0),2,2)
Sample2 <- data.frame(1:2,c(1,0),1)
names(Sample2) <- c("Team","WinsTotal","Strength")
Sample2$Versus <- vs
Sample2$WinsVersus <- vw
Sample2

vs <- matrix(c(0,2,
               2,0),2,2)
vw <- matrix(c(0,1,
               1,0),2,2)
Sample3 <- data.frame(1:2,c(1,0),1)
names(Sample3) <- c("Team","WinsTotal","Strength")
Sample3$Versus <- vs
Sample3$WinsVersus <- vw
Sample3

vs <- matrix(c(0,1,0,
               1,0,1,
               0,1,0),3,3)
vw <- matrix(c(0,0,0,
               1,0,0,
               0,1,0),3,3)
Sample4 <- data.frame(1:3,c(1,1,0),1)
names(Sample4) <- c("Team","WinsTotal","Strength")
Sample4$Versus <- vs
Sample4$WinsVersus <- vw
Sample4

vs <- matrix(c(0,1,0,0,0,0,0,
               1,0,1,0,0,0,0,
               0,1,0,1,0,0,0,
               0,0,1,0,1,0,0,
               0,0,0,1,0,1,0,
               0,0,0,0,1,0,1,
               0,0,0,0,0,1,0),7,7)
vw <- matrix(c(0,0,0,0,0,0,0,
               1,0,0,0,0,0,0,
               0,1,0,0,0,0,0,
               0,0,1,0,0,0,0,
               0,0,0,1,0,0,0,
               0,0,0,0,1,0,0,
               0,0,0,0,0,1,0),7,7)
Sample5 <- data.frame(1:7,c(1,1,1,1,1,1,0),1)
names(Sample5) <- c("Team","WinsTotal","Strength")
Sample5$Versus <- vs
Sample5$WinsVersus <- vw
Sample5

vs <- matrix(c(0,1,1,1,
               1,0,2,1,
               1,2,0,1,
               1,1,1,0),4,4)
vw <- matrix(c(0,0,0,0,
               1,0,1,0,
               1,1,0,0,
               1,0,0,0),4,4)
Sample6 <- data.frame(1:4,c(3,2,1,0),1)
names(Sample6) <- c("Team","WinsTotal","Strength")
Sample6$Versus <- vs
Sample6$WinsVersus <- vw
Sample6

sim.t1 <- data.frame(c("Red","Ethan","May","Lucas","Hilbert","Rosa","Serena")
                     ,c(1,1.1,0.9,1,1,1.1,0.9))
vs <- matrix(c(0 ,15,10,5 ,5 ,15,0 ,
               15,0 ,0 ,10,0 ,0 ,0 ,
               10,0 ,0 ,0 ,0 ,0 ,10,
               5 ,10,0 ,0 ,0 ,0 ,0 ,
               5 ,0 ,0 ,0 ,0 ,10,0 ,
               15,0 ,0 ,0 ,10,0 ,0 ,
               0 ,0 ,10,0 ,0 ,0 ,0 ),7,7)
names(sim.t1) <- c("Team","Strength")
sim.t1$Versus <- vs
sim.t1$Team <- as.character(sim.t1$Team)
sim.t1

false.t1 <- data.frame(c("Red","Ethan","May","Lucas","Hilbert","Rosa","Serena")
                       ,c(1,1,1,1,1,1,1))
vs <- matrix(c(0 ,1 ,2 ,2 ,2 ,1 ,0 ,
               1 ,0 ,0 ,2 ,0 ,0 ,0 ,
               2 ,0 ,0 ,0 ,0 ,0 ,2 ,
               2 ,2 ,0 ,0 ,0 ,0 ,0 ,
               2 ,0 ,0 ,0 ,0 ,2 ,0 ,
               1 ,0 ,0 ,0 ,2 ,0 ,0 ,
               0 ,0 ,2 ,0 ,0 ,0 ,0 ),7,7)
vw <- matrix(c(0 ,0 ,1 ,1 ,1 ,0 ,0 ,
               1 ,0 ,0 ,1 ,0 ,0 ,0 ,
               1 ,0 ,0 ,0 ,0 ,0 ,1 ,
               1 ,1 ,0 ,0 ,0 ,0 ,0 ,
               1 ,0 ,0 ,0 ,0 ,1 ,0 ,
               1 ,0 ,0 ,0 ,1 ,0 ,0 ,
               0 ,0 ,1 ,0 ,0 ,0 ,0 ),7,7)
names(false.t1) <- c("Team","Strength")
false.t1$Versus <- vs
false.t1$WinsVersus <- t(vw)
false.t1$Team <- as.character(false.t1$Team)
false.t1

#go <- proc.time()
#Team <- vector()
#Versus <- vector()
#N <- nrow(NBAdf)
#for (i in 1:N^2) {
#  if (NBAdf$WinsVersus[i] != 0) {
#    reps <- NBAdf$WinsVersus[i]
#    for (j in 1:reps) {
#      Team <- c(Team,NBAdf$Team[ifelse(i%%N==0,N,i%%N)])
#      Versus <- c(Versus,NBAdf$Team[ifelse(i%%N==0,i%/%N,i%/%N+1)])
#    }
#  }
#}
#VisualComp <- data.frame(Winner=Team,Versus,BradleyTerry=NBAComp$BradleyTerryProbabilities
#           ,Mosteller=NBAComp$MostellerProbabilities)
#VisualComp$Model <- ifelse(VisualComp$BradleyTerry>VisualComp$Mosteller,"BT","M")
#proc.time() - go
