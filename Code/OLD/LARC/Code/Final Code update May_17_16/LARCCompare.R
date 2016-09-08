#This file contains a function that drops each game from the data provided one at a time and then
# uses the remaining data to compare the two models and see which one is more successful at
# predicting the actual result of the game. It only takes one arguement in the form of a data frame.
#The function returns tables comparing the two models and which one predicted more accurately more
# often as well as a small data frame with a summary of all the results.
LARC.Compare <- function(df) {
  BTP <- c(LARC.Prob(df, type = "BT"),0,1,0.5)
  MP <- c(LARC.Prob(df, type = "M"),1,0,0.5)
  cmprsn <- table(ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")))-1
  prcntgs <- prop.table(cmprsn)
  ss <- sum(df$WinsVersus)
  BTP <- BTP[1:ss]
  MP <- MP[1:ss]
  Team <- vector()
  Versus <- vector()
  N <- nrow(df)
  #This loop drops the games and guesses the results
  for (i in 1:N^2) {
    if (df$WinsVersus[i] != 0) {
      reps <- df$WinsVersus[i]
      for (j in 1:reps) {
        Team <- c(Team,df$Team[ifelse(i%%N==0,N,i%%N)])
        Versus <- c(Versus,df$Team[ifelse(i%%N==0,i%/%N,i%/%N+1)])
      }
    }
  }
  VisualComp <- data.frame(Winner=Team,Versus,BradleyTerryProbabilities=BTP,MostellerProbabilities=MP,
                           Model=ifelse(BTP>MP,"Bradley-Terry",ifelse(BTP<MP,"Mosteller","Tie")),
                           Difference=MP-BTP)
  #this gives the results a specific type so only some results show unless directed otherwise
  lst <- list(Comparison=cmprsn,Proportion=prcntgs,DataTable=VisualComp)
  class(lst) = "prthdn"
  return(lst)
}

#this isn't a typical funcion and creates the results style mentioned above, but does not function
# if run on its own.
print.prthdn <- function(x, ...){
  x = x["Proportion"]
  NextMethod()
}

#this is an example using the sim1 created in the Partial Data simulator file
(fun <- LARC.Compare(sim1))
fun$Comparison
fun$DataTable

(employeeprob <- LARC.Compare(Employeedf))
employeeprob$Comparison
employeeprob$DataTable
