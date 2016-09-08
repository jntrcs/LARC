# function for changing the home court advantage parameter
change.h = function( dir, post, s, h ) {
    # Oct 25, 2012 
    # dir -- dir =  1 indicates we are to increase h
    #        dir = -1 indicates we are to decrease h
    # post -- the latest value of the posterior distribution
    # s    -- a vector of the t team strengths
    # h    -- the lastest value of home court parameter
    
    # global variable MC defined in Bradley.Terry
        
    prior.strengths = prod(s)*exp(-sum(s))
                              # we calculate this only once 
    
    hnew = xp1( h + dir*incr)  #for h >= 1   
    
    newpost = 
        
    MC*prior.strengths*postBT.minus.strengths( s, hnew)
    
    while( newpost > post)  {        # as long the posterior keeps getting larger, 
                                     # we will continue
        post = newpost
        hnew = xp1( hnew + dir*incr)
        newpost = 
        MC*prior.strengths*postBT.minus.strengths( s, hnew)
    }  
    #end while
    
    hnew = hnew - dir*incr
    
    return( hnew )
    
}                                    #end function
# Function tested on the 2 game 3 team situation
# 1(H) > 2; 2(H) > 3
# h = 2,3429