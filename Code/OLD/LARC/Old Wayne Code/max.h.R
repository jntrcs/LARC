# function for maximizing h, the home court advantage parameter
max.h = function( post, s, h) {
    # Oct 25, 2012
    # post is the latest value of the posterior distribution. 
    #     We are maximizing this value
    # s -- a vector of size t -- the latest strengths estimates. 
    #     max.h will NOT change these
    # h -- the latest estimate of the home court advantage -- h >= 1.0
   
    h.up   = h + incr
    postup = postBT( s, h.up )
    
    if( postup > post) { #incrementing h improves the posterior
        
        hnew = change.h( 1, post, s, h.up )
        
        return( hnew )       
        
    } else {            #see if decreasing h improves the posterior
        
        h.down = xp1( h - incr)
        
        postdown = postBT( s, h.down )
        
        if (postdown > post) { # decreasing h improves the posterior
            
            hnew = change.h( -1, post, s, h.down )
            
            return( hnew )
            
        } else {        # the posterior is already maximized with respect to h
            
            return (h)  # post and h are unchanged
            
        }               # end of the inner else
        
    }                   # end of the outer else
    
}                       # end of the function 

# note there are three different ways to return in this function

# This function was tested in the 2 games, 3 team situation
# 1(H) > 2; 2(H) > 3
# all strenghts = 1.
# 3 different h's, 1.5, 2.3429, 3
# all resulted in a h of 2.3429
