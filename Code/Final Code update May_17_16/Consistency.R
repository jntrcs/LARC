##Similarity test
#The purpose of this test is to compare new results to old results, so that I can make changes to the 
#calculation functions and I can ensure it doesn't break the existing numbers
load("MasterFunctionFile.RData")
load("2015FootballData.RData")

system.time(
newBTStrengths<-LARC.Rank(all2015data[[5]][[1]])
)
cbind(newBTStrengths, all2015data[[5]][[2]])
stopifnot(newBTStrengths$Strength==all2015data[[5]][[2]]$Strength)
system.time(
newTMStrengths<-LARC.Rank(all2015data[[5]][[1]], func=ThurstoneMostellerLARC)
)
cbind(newTMStrengths, all2015data[[5]][[3]])
stopifnot(newTMStrengths$Strength==all2015data[[5]][[3]]$Strength)
