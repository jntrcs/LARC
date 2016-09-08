# function for computing the value of the Bradley-Terry posterior for values of the parameters.
# Oct 23, 2012
postBT = function(s, h ) {
   # postBT is short hand for posterior distribution for the Bradley-Terry    
   # s is a vector of length t -- the strengths of the t teams; s[i] > 0    
   # h is the home court advantage; h > 1
   # the following global variables are defined in Bradley.Terry 
   #  g, games, t
    
    prior.strengths = prod(s)*exp(-sum(s)) 
        # calculate the value of the prior distribution for the t strengths
    
    prior.h         = (h-1)*exp(-(h-1)) 
        # calculate the value of the prior distribution for the home court factor
    
    prior = prior.strengths * prior.h
    
    cond = 1                            
        # calculate the value of conditional distribution
        # in the for statement below
    
    for( i in 1:g) { 
        winner.strength = s[games[i,1]]* ifelse( games[i,3] == 'H', h, 1)
        loser.strength  = s[games[i,2]]* ifelse( games[i,3] == 'A', h, 1) 
        cond = cond * winner.strength/(winner.strength + loser.strength )
    }

        postBT = MC * prior * cond 
       # The final value of the posterior for s and h
       # The term MC is just to make the value of prior more readable
}  
# Function tested on Oct 23, 2012 for one game home team winning
# also for 2 games -- 1 (H) > 2; 2 (H) > 3