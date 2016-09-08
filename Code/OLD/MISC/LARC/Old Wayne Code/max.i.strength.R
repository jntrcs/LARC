# function for maximizing s[i], the strength of the ith team
max.i.strength = function ( post, i, s, h ) { 
    # Oct 29, 2012
    # post = the value of the posterior at this stage in the maximize process
    # i    = the index of the strength that is being maximized holding all 
    #        other parameters constant
    # s    = a vector of the latest t strength estimates
    
    # the global variables games, incr, and t are described in Bradley.Terry
    
s[i] = s[i] + incr

postup = postBT( s, h )  # compute a new value for the posterior
                         # incrementing s[i] by incr

if ( postup > post) {
     s[i] = change.i.strength( 1, post, i, s, h ) 
                         # if incrementing s[i] increases the value of the 
                         # posterior, invoke change.i.strength to see
                         # how much we should increase s[i] 
     return( s[i] )                
   } else {
       s[i] = xp0( s[i] - 2* incr) 
                         # decrease s[i] making sure it does not got below zero.  
       
            postdown = postBT( s, h )
                         # compute a new value for the posterior decreasing
                         # the original value of s[i] by incr
            
            if( postdown > post) { 
                s[i]  = change.i.strength ( -1, post, i,   s, h ) 
                         # if decreasing s[i] increases the value of 
                         # the posterior, invoke change.i.strength to see
                         # how much we should decrease s[i]
                return(s[i]) 
                                   
            } else {     # if neither increasing or decreasing s[i]
                         # improves posterior,
                         # s[i] remains unchanged.
                
                    s[i] = s[i] + incr
                    
                    return(s[i])
                         
            }            # end of inner else branch        
   }                     # end of the outer else branch
}                        # end of function maxstrengths
                         
# note this function can be returned in one of three ways

# function has been tested for a 3 team, 2 game situation
# 1 (H) > 2
# 2 (H) > 3
# i = 1, s[1] = 1.3333
# i = 3, s[3] = 0.6861
# i = 2, h = 1, s[2] = 1