# function for changing the ith strenght parameter
change.i.strength = function( dir, post, i, s, h ) {
    # Oct 24 2012 
    # dir -- dir =  1 indicates we are to increase s[i]
    #        dir = -1 indicates we are to decrease s[i]
    # post -- the latest value of the posterior distribution
    # i    -- which team's strength is being modified
    # s    -- a vector of the t team strengths
    # h    -- the lastest value of home court parameter
   
    # the global variables g, games, incr, MC and t 
    # are described in Bradley.Terry
 
    s.less.i = s[-i] # create a vector that has all the strengths
                     # except for the ith one
    
    prior.less.i = prod(s.less.i)*exp(-sum(s.less.i))*(h-1)*exp(-(h-1))
                     # the prior calculation for all parameters 
                     # except for the ith strength
    
    cond.less.i = 1  # now we compute the conditional for 
                     # all paramters except the ith strength

    for( j in 1:g)  {
        
        if (!games[j,1]==i & !games[j,2]==i ){
            #the ith team does not play in this game
            winner.strength = s[games[j,1]]*ifelse(games[j,3]=='H',h,1)
            loser.strength  = s[games[j,2]]*ifelse(games[j,3]=='A',h,1)
            cond.less.i = 
            cond.less.i *  winner.strength / ( winner.strength + loser.strength)
        }                                      # end if
    }                                          # end for    

    post.less.i = prior.less.i * cond.less.i
    
# We just computed ONCE the posterior for all elements which do NOt contain s[i]
# we now create an array. games.i which contains all the rows of games
# for which the ith team plays
    
    games.i = NULL
    
    for ( j in 1:g) {
        if( (games[j,1] == i | games[j,2] == i) ) { 
            games.i = rbind(games.i,games[j,] )
        }   # end if   
    }       # end for
    
    s[i] = xp0( s[i] + dir*incr)    
    
    newpost =
    MC*post.less.i * s[i]*exp(-s[i])*cond.i(s,h,games.i)

    while( newpost > post)  {  # as long the posterior keeps getting larger,
                               # we will continue
        post = newpost
        s[i] = xp0( s[i] + dir*incr)
        newpost = 
        MC*post.less.i * s[i]*exp(-s[i])*cond.i(s,h,games.i)
    }
    
    #end while
    
    s[i] = s[i] - dir*incr
    
    return( s[i] )
    
}                                    #end function
# Function tested on 2 game 3 team situation 
# 1 (H) > 2; 2 (H) > 3
# i = 1 s[1] = 1.3333
# i = 3 s[3] = 0.6861
# i =2, h = 1, s[2] = 1