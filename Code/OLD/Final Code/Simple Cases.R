BradleyTerryLARC <- function(strengths,wins,magnificationfactor=1) {
  PI <- 1
  PIPI <- 1
  W <- vector()
  x <- 0
  for (i in 1:length(strengths)) {
    W[i] <- sum(wins[i,])
    PI <- PI*strengths[i]^(W[i]+1)
    for (j in (i+1):length(strengths)) {
      if (j < length(strengths)+1) {
        x <- x + 1
        PIPI <- PIPI*(1/(strengths[i]+strengths[j])^(wins[i,j]+wins[j,i]))*magnificationfactor
      }
    }
  }
  return(exp(-sum(strengths))*PI*PIPI)
}

MostellerLARC <- function(strengths,wins,magnificationfactor=1) {
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

LARC.Posterior <- function(df, func = BradleyTerryLARC, mf=1, adj=1, report=FALSE) {
  posterior <- func(df$Strength,df$WinsVersus,mf)
  while (posterior == 0) {
    mf <- mf + adj
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  while (posterior == Inf) {
    mf <- mf - adj
    posterior <- func(df$Strength,df$WinsVersus,mf)
  }
  if (report==TRUE){
    return(c(posterior,mf))
  } else {
    return(posterior)
  }
}

LARC.Optim <- function(df, func = BradleyTerryLARC, increment = 0.001,
                       iterations = Inf, magnificationfactor=1, adj=1) {
  st <- df$Strength
  wv <- df$WinsVersus
  
  comp <- LARC.Posterior(df, func, mf = magnificationfactor, adj = adj)
  options(digits = min(which( increment*10^(0:20)==floor(increment*10^(0:20)) )) - 1)
  inc <- 0.1
  last <- Inf
  x <- 0
  while ((comp != last & x < iterations) | inc >= increment) {
    last <- comp
    for (i in 1:nrow(df)) {
      df$Strength[i] <- df$Strength[i] + inc
      new <- LARC.Posterior(df, func, mf = magnificationfactor,adj=adj)
      if (comp > new) {
        df$Strength[i] <- df$Strength[i] - 2*inc
        new <- LARC.Posterior(df, func, mf = magnificationfactor,adj=adj)
        if (comp > new) {
          df$Strength[i] <- df$Strength[i] + inc
        } else {
          comp <- new
        }
      } else {
        comp <- new
      }
    }
    if (comp == last) {
      inc <- inc/10
    }
    x <- x + 1
  }
  return(list(UpdatedStrengths=df$Strength,MaximizedPosterior=comp,Iterations=x))
}

LARC.Rank <- function(df, func=BradleyTerryLARC, increment = 0.001, 
                      iterations = Inf, dgt=3, magnificationfactor=1, adj=1) {
  options(digits=dgt)
  tt <- nrow(df)
  
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

vs <- matrix(c(0,1,
               1,0),2,2)
vw <- matrix(c(0,0,
               1,0),2,2)
Sample1 <- data.frame(1:2,c(1,0),1)
names(Sample1) <- c("Team","WinsTotal","Strength")
Sample1$Versus <- vs
Sample1$WinsVersus <- vw
Sample1

LARC.Rank(Sample1,BradleyTerryLARC)

#change the initial strengths to be zero

Sample1$Strength <- c(0,0)

LARC.Rank(Sample1,MostellerLARC)

vs <- matrix(c(0,2,
               2,0),2,2)
vw <- matrix(c(0,0,
               2,0),2,2)
Sample2 <- data.frame(1:2,c(1,0),1)
names(Sample2) <- c("Team","WinsTotal","Strength")
Sample2$Versus <- vs
Sample2$WinsVersus <- vw
Sample2

LARC.Rank(Sample2,BradleyTerryLARC)

# Change the strengths to zero

Sample2$Strength <- c(0,0)

LARC.Rank(Sample2,MostellerLARC)

vs <- matrix(c(0,2,
               2,0),2,2)
vw <- matrix(c(0,1,
               1,0),2,2)
Sample3 <- data.frame(1:2,c(1,0),1)
names(Sample3) <- c("Team","WinsTotal","Strength")
Sample3$Versus <- vs
Sample3$WinsVersus <- vw
Sample3

LARC.Rank(Sample3,BradleyTerryLARC)

# change the strengths to 0.0

Sample3$Strength <- c(0,0)

LARC.Rank(Sample3,MostellerLARC)

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

LARC.Rank(Sample4,BradleyTerryLARC)

# set Strengths to zero

Sample4$Strength <- c(0,0,0)

LARC.Rank(Sample4,MostellerLARC)

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

LARC.Rank(Sample5,BradleyTerryLARC)

# Change initial strenghts to zero

Sample5$Strength <- c(0,0,0,0,0,0,0,0)

LARC.Rank(Sample5,MostellerLARC)

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

LARC.Rank(Sample6,BradleyTerryLARC)

# Change intial strength to zero

Sample6$Strength <- c(0,0,0,0)

LARC.Rank(Sample6,MostellerLARC)
