#These are tests to see if it matters where the strengths of the Thurstone-Mosteller model are
# centered or not.

NBAR <- LARC.Optim(NBAdf,ThurstoneMostellerLARC)$UpdatedStrengths

NBAdfzero <- NBAdf
NBAdfzero$Strength <- 0

NBARZ <- LARC.Optim(NBAdfzero,ThurstoneMostellerLARC)$UpdatedStrengths

ThurstoneMostellerLARC.ONE <- function(strengths,wins,magnificationfactor=1) {
  # First we compute the Prior
  prior <- 1
  for (i in 1: length(strengths)) { prior = prior * dnorm( strengths[i])*magnificationfactor}
  # Now we compute the conditional
  cond <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      cond <- cond*pnorm(strengths[i]-strengths[j])^wins[i,j]*magnificationfactor
    }
  }
  # Now put the two together 
  post = prior * cond
  return(post)
}

NBARO <- LARC.Optim(NBAdf,ThurstoneMostellerLARC.ONE)$UpdatedStrengths
NBARZO <- LARC.Optim(NBAdfzero,ThurstoneMostellerLARC.ONE)$UpdatedStrengths

ThurstoneMostellerLARC.ZERO <- function(strengths,wins,magnificationfactor=1) {
  PIPI <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      PIPI <- PIPI*pnorm(strengths[i]-strengths[j],0,0)^wins[i,j]*magnificationfactor
    }
  }
  return(PIPI)
}

NBAR0 <- LARC.Optim(NBAdf,ThurstoneMostellerLARC.ZERO)$UpdatedStrengths
NBARZ0 <- LARC.Optim(NBAdfzero,ThurstoneMostellerLARC.ZERO)$UpdatedStrengths


cbind(NBAR,NBARZ,NBARO,NBARZO,NBAR0,NBARZ0)
