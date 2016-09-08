# function for computing the value of the Bradley-Terry posterior for values of the parameters.
postBT.minus.strengths = function( s, h ) {
   # Oct 25, 2012    
   # s is a vector of length t -- the strengths of the t teams; s[i] > 0    
   # h is the home court advantage; h > 1

   # global variables g, games, incr defined in Bradley.Terry    
      
    prior.h = (h-1)*exp(-(h-1))  # calculate the value of the prior
                                 # distribution for the home court factor
 
    cond = 1                     # calculate the value of conditional distribution
                                 # in the for statement below
    
    for( i in 1:g) { 
        winner.strength = s[games[i,1]]* ifelse( games[i,3] == 'H', h, 1)
        loser.strength  = s[games[i,2]]* ifelse( games[i,3] == 'A', h, 1) 
        cond = cond * winner.strength/(winner.strength + loser.strength )
    }
        postBT.minus.strenght =  prior.h * cond  
                                # The final value of the posterior for h
    
}                               # end function

# function has been tested for the two game 3 team situation
# 1 (H) > 2, 2 (H) > 3
# value = 0.1091755