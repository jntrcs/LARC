# function for computing the value of the Bradley-Terry conditional
# for only games involving the ith team
# Oct 24, 2012

cond.i = function( s, h, games.i) {
   # s is a vector of length t -- the strengths of the t teams; s[i] > 0    
   # h is the home court advantage; h > 1
   # games.i is a table -- 3 columns -- the length is the number 
   # of games in the schedule involving the ith team
      # col 1 -- the team number of the winning team
      # col 2 -- the team number of the losing team
      # col 3 -- where was the game played
         # 'H' indicates that game was played on the winning team's court
         # 'A' indicates that game was played on the losing  team's court
 

    g.i = length(games.i[,1])    # how many games for the ith team
 
    cond = 1                     # calculate the value of conditional
                                 # distribution in the for statement below
    
    for( j in 1:g.i) { 
        
        winner.strength = s[games.i[j,1]]* ifelse( games.i[j,3] == 'H', h, 1)
        loser.strength  = s[games.i[j,2]]* ifelse( games.i[j,3] == 'A', h, 1) 
        cond = cond * winner.strength/(winner.strength + loser.strength )

        }       # end for   
    
        return( cond )
                                                     
}               # end function   
# This function has been tested with one game 1 (H) > 2
