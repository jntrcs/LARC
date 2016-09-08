#LARC.Rank: this function optimizes the LARC strengths and then creates a new dataframe
# that includes those new strengths and ranks the teams according to them. It only needs a
# dataframe, but can also take a function to optimize over and the number of decimals that
# should be reported for the updated strengths(the default is BradleyTerryLARC). If it fails
# the adj value should be made smaller so it can pinpoint a magnification factor that is widely
# relevant.
LARC.Rank <- function(df, func=BradleyTerryLARC, increment = 0.001, 
                      iterations = Inf, dgt=3, magnificationfactor=1, adj=1) {
  options(digits=dgt)
  tt <- nrow(df)
  length_strength <- length(df$Strength) # Get the length of strength
  if_mosteller <- identical(func,ThurstoneMostellerLARC) # Check if MostellerLARC function is used
  if (if_mosteller == TRUE){
    for (i in 1:length_strength){
      df$Strength[i] <- 0}# if MostellerLARC then change the initial strength to be Zero
  }
  
  if (is.null(df$WinsTotal)) {
    for (i in 1:tt) {
      df$WinsTotal[i] <- sum(df$WinsVersus[i,])
    }
  }
  
  optimized <- LARC.Optim(df, func, increment, iterations, magnificationfactor,adj=adj)
  df$UpdatedStrength <- round(optimized$UpdatedStrengths,dgt)
  
  TempOrder <- df[order(-df$UpdatedStrength,-df$WinsTotal),]
  Ranked <- data.frame(1:tt,TempOrder$Team,TempOrder$UpdatedStrength,TempOrder$WinsTotal)
  names(Ranked) <- c("Rank","Team","Strength","WinsTotal")
  
  Ranking <- data.frame(Ranked$Rank,as.character(Ranked$Team),Ranked$Strength,Ranked$WinsTotal)
  names(Ranking) <- c("Rank","Team","Strength","WinsTotal")
  return(Ranking)
}

#an example of this function creating a Ranking dataframe named NBARanking
NBARanking <- LARC.Rank(NBAdf)