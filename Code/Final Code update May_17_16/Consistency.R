##Similarity test
#The purpose of this test is to compare new results to old results, so that I can make changes to the 
#calculation functions and I can ensure it doesn't break the existing numbers
load("MasterFunctionFile.RData")
load("2015FootballData.RData")

system.time(
newBTStrengths<-LARC.Rank.Football(all2015data[[5]][[1]])
)
cbind(newBTStrengths, all2015data[[5]][[2]][, 1:3])
#newBTStrengths$Strength==all2015data[[5]][[2]]$Strength
system.time(
newTMStrengths<-LARC.Rank.Football(all2015data[[5]][[1]], func=ThurstoneMostellerLARC, inc=.01)
)
cbind(newTMStrengths, all2015data[[5]][[3]][,1:3])
stopifnot(newTMStrengths$Strength==all2015data[[5]][[3]]$Strength)

newBTStrengths$Strength==all2015data[[5]][[2]]$Strength
#OrigTime<-system.time(
 # EmployeeRankOriginal <- LARC.Rank(Employeedf)
#)
#OrigTimeTM<-system.time(
 # EmployeeRankOriginalTM <- LARC.Rank(Employeedf, func=ThurstoneMostellerLARC)
#)
#OrigTime
#EmployeeRankOriginal
#EmployeeRankOriginalTM
#save(Employeedf, OrigTime, EmployeeRankOriginal, EmployeeRankOriginalTM, OrigTimeTM, file="EmployeeOriginals.RData")

load("EmployeeOriginals.RData")
newTime<-system.time(
  EmployeeRankNew <- LARC.Rank(Employeedf)
)
newTimeTM<-system.time(
  EmployeeRankNewTM <- LARC.Rank(Employeedf, func=ThurstoneMostellerLARC)
)
cbind(EmployeeRankOriginal, EmployeeRankNew)
stopifnot(EmployeeRankOriginal$Team==EmployeeRankNew$Team)
stopifnot(EmployeeRankOriginal$Strength==EmployeeRankNew$Strength)
stopifnot(EmployeeRankOriginalTM$Team==EmployeeRankNewTM$Team)
stopifnot(EmployeeRankOriginalTM$Strength==EmployeeRankNewTM$Strength)
print ("New    Old")
print(c(newTime[3], OrigTime[3]))
print(c(newTimeTM[3], OrigTimeTM[3]))

