# function for optomizing the Bradley-Terry Model
optomize.par = function() {
    # Oct 29, 2012
    # The following global variables are defined in Bradley-Terry
    # h, s, t

   snew = s
   
   maxreached = rep(c(F),times=t)
                       # we have not reached a maximun for
                       # for any of the t strengths
     
   maxhreached    = FALSE      
                       # we have not reached the maximum
                       # for the home court value
  
  iter = 0             # how many iterations do we have
   
  pmaxr = 0            # pmaxr is short hand for posterior maximun reached
                       
   
                       # the code below checks to see if 
                       # a maximum appears to be reached and 
                       # repeats itself four times we will stop the iterations

while ( (!all(maxreached) | !maxhreached ) & iter < 30 & pmaxr < 4) { 
                       #if any of the t-maxreached values is FALSE, 
                       #all(maxreache) is FALSE
    
                       # this while loop will continue as long as
                       # at least one strength estimate or 
                       # the home court advantage has not reached 
                       # a maximun
    
                       # also if the calculated posterior is the 
                       # same (to five decimal digits accuracy)
                       # four times in a row, we exit the while loop
    
                       # finally we will exit while if we take over
                       # 200 iterations
      
     iter = iter + 1
             
     if( iter == 1) {
         post = postBT( s, h)  
                          # first time while loop compute posterior
     } else {
         post = newpost   # otherwise, post is equal to newpost
     }
                         
     for ( i in 1:t) {
         snew[i] = max.i.strength(post,i,s,h)
         
         if ( snew[i] - s[i] == 0) { maxreached[i] = T}  
                          # we have reached a maximun with respect to s[i]
         
     }                               #end for 
     
     hnew = max.h(post,s,h)
     
     if ( hnew - h == 0) { maxhreached = T }
                        # we have reached a maximum with respect to h
     newpost = postBT( snew, hnew )
     s = snew
     h = hnew
     
     if( round( post, digits = 4) == round(newpost, digits = 4)) {
                        # have we reached a maximum with respect to post?
      pmaxr = pmaxr + 1
      
     } else {
         pmaxr = 0
     }                  # end else
   
    
  }                     # end while
  
  result = list( s = s, h = h, post = post, n.iter = iter)
  
  return(result)
}                       # end function      