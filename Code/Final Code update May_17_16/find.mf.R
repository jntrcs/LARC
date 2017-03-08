# find.mf -- function finds the magnification factor
# 
#    input parameters described below
#
#    df -- the data frame of the input data
#          see LARC.optim for a description of this data frame
#    mf -- orginal magnification factor set equal to 1
#     
#    func - which model is being used, BT or M, default is BradleyTerryLARC
#
#    adj  -- adjustment factor, orginally set equal to 1
#
#  when the posterior estimates get very small, essentially zero, this function keeps increasing the mf.
#  when the posterior estimates get too large, this function decreases the mf
# 
#This function is a tool to be used within other functions to simply find the appropriate 
# magnification factor to prevent rounding to zero of infinity to break the calculation. 
#This function is not necessary if you use the log density instead of the  density

find.mf <- function(df, mf = 1, func, adj=1) {
  mf<-1
  d<-func(df$Strength,df$WinsVersus, df$WinsTotal,mf)
  x<-0
  while (d<1e-100|d>1e100)
  {
    x<-x+1
    if (d<1e-100) {
      mf<-mf+adj
      if (func(df$Strength,df$WinsVersus, df$WinsTotal,mf) >1e100)
        adj<-adj/10
    }
    else if (d>1e100)
    {
      mf<-mf-adj
      if (func(df$Strength,df$WinsVersus, df$WinsTotal,mf) <1e-100)
        adj<-adj/10
    }
    d<-func(df$Strength,df$WinsVersus, df$WinsTotal,mf)
    stopifnot(x<1000)
   
  }
  return(mf)
}