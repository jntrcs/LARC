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
find.mf <- function(df, mf = 1, func = BradleyTerryLARC, adj=1) {
  if (func(df$Strength,df$WinsVersus,mf) == 0) {
    while (func(df$Strength,df$WinsVersus,mf) == 0) {
      mf <- mf + adj
    }
    mfo <- round(mf*10)
    while (func(df$Strength,df$WinsVersus,mfo) == Inf) {
      mfo <- mfo - adj
    }
    mf <- mean(c(mf,mfo))
  } else { 
    if (func(df$Strength,df$WinsVersus,mf) == Inf) {
      while (func(df$Strength,df$WinsVersus,mf) == Inf) {
        mf <- mf - adj
      }
      mfo <- round(mf/10)
      while (func(df$Strength,df$WinsVersus,mfo) == 0) {
        mfo <- mfo + adj
      }
      mf <- mean(c(mf,mfo))
    }
  }
  return(mf)
}