
NBAR <- LARC.Optim(NBAdf,MostellerLARC)$UpdatedStrengths

NBAdfzero <- NBAdf
NBAdfzero$Strength <- 0

NBARZ <- LARC.Optim(NBAdfzero,MostellerLARC)$UpdatedStrengths

MostellerLARC.ONE <- function(strengths,wins,magnificationfactor=1) {
  PIPI <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      PIPI <- PIPI*pnorm(strengths[i]-strengths[j],1,1)^wins[i,j]*magnificationfactor
    }
  }
  return(PIPI)
}

NBARO <- LARC.Optim(NBAdf,MostellerLARC.ONE)$UpdatedStrengths
NBARZO <- LARC.Optim(NBAdfzero,MostellerLARC.ONE)$UpdatedStrengths

MostellerLARC.ZERO <- function(strengths,wins,magnificationfactor=1) {
  PIPI <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      PIPI <- PIPI*pnorm(strengths[i]-strengths[j],0,0)^wins[i,j]*magnificationfactor
    }
  }
  return(PIPI)
}

NBAR0 <- LARC.Optim(NBAdf,MostellerLARC.ZERO)$UpdatedStrengths
NBARZ0 <- LARC.Optim(NBAdfzero,MostellerLARC.ZERO)$UpdatedStrengths


cbind(NBAR,NBARZ,NBARO,NBARZO,NBAR0,NBARZ0)
