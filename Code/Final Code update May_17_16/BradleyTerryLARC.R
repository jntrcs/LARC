#This file includes six  functions:

#BradleyTerryLARC 
# this function is the Bradley-Terry Model.
# The function computes the posterior for the model given the strengths and the wins' schedule
# the function has three arguements
# 
# strengths -- a vector of the team's strengths
# wins      -- a matrix of the number of times that team i beat team j
# magnificationfactor -- when the posterior becomes too small this factor increases the size of the posterior
#
BradleyTerryLARC <- function(strengths,wins,magnificationfactor=1) {
  PI <- 1
  PIPI <- 1
  W <- vector()
  x <- 0 #Never Used
  for (i in 1:length(strengths)) {
    W[i] <- sum(wins[i,])
    PI <- PI*strengths[i]^(W[i]+1)
    for (j in (i+1):length(strengths)) {
      if (j < length(strengths)+1) {
        x <- x + 1 #This is unused so I'm not sure why it's here.
        PIPI <- PIPI*(1/(strengths[i]+strengths[j])^(wins[i,j]+wins[j,i]))*magnificationfactor
      }
    }
  }
  return(exp(-sum(strengths))*PI*PIPI)
}
#
#an example of the function
#BradleyTerryLARC(NBAdf$Strength,NBAdf$WinsVersus)
#the next four lines are to give a comparison of what the preceeding function should return
# y <- 0
# for (i in 1:30) {
#  y <- y + sum(NBAdf$Versus[i,i:30])
#}
#exp(-30)*1*(1/2^y)
