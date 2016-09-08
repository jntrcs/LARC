str1 <- 1.5
str2 <- 1.0
str3 <- 0.5

#CDF - F(x) inputing the strength for x
punif(str1,0,10)
punif(str2,0,10)
punif(str3,0,10)
#this method just converts the strength to a decimal value

#CDF - F(x) inputing the strength for one team minus the strength for the other as x
punif(str1-str2,0,10)
punif(str2-str1,0,10)
punif(str3-str1,0,10)
punif(str1-str3,0,10)
punif(str3-str2,0,10)
punif(str2-str3,0,10)
#this way has all strengths as .1 or less

#PDF - f(x) inputing the strength for x
dunif(str1,0,10)
dunif(str2,0,10)
dunif(str3,0,10)
#this method gives every team a probability of .1 of winning which isn't sound with only two teams

#PDF - f(x) inputing the strength for one team minus the strength for the other as x
dunif(str1-str2,0,10)
dunif(str2-str1,0,10)
dunif(str3-str1,0,10)
dunif(str1-str3,0,10)
dunif(str3-str2,0,10)
dunif(str2-str3,0,10)
#in this method the weaker team has 0 probability and the stronger team always has .1

#CDF - F(x) inputing the strength for x but using the sum of the strengths as the upper bound
punif(str1,0,sum(str1,str2))
punif(str2,0,sum(str1,str2))
punif(str1,0,sum(str1,str3))
punif(str3,0,sum(str1,str3))
punif(str2,0,sum(str3,str2))
punif(str3,0,sum(str2,str3))
# this method looks fine until you realize it is identical to the bradley-terry

#CDF - F(x) inputing the strength for x but using the max strength as the upper bound and
# the minimum strength as the lower bound
punif(str1,min(str1,str2),max(str1,str2))
punif(str2,min(str1,str2),max(str1,str2))
punif(str1,min(str1,str3),max(str1,str3))
punif(str3,min(str1,str3),max(str1,str3))
punif(str2,min(str2,str3),max(str2,str3))
punif(str3,min(str2,str3),max(str2,str3))
# this method always has the stronger team winning
