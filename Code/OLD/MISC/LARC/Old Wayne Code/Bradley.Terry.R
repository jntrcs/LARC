# Function for computing the strengths of t teams using the Bradly-Terry Model 
# Nov 1, 2012
source('change.h.R')
source('change.i.strength.R')
source('cond.i.R')
source('max.h.R')
source('max.i.strength.R')
source('optomize.par.R')
source('postBT.R')
source('postBT.minus.strengths.R')
source('xp0.R')
source('xp1.R')

# the working directory needs to be set to LARC

incr = 0.0001  # what precision do I want to compute 
               # the final paramter estimates.

t    = 16       # the number of teams 
       
team.strengths = read.csv('StrengthsBT.csv', header = T, sep = ',') 
                # team.strengths is a 3 variable data file
                #   V1 = name -- the name of the team
                #   V2 = number -- the team number
                #   V3 = strength -- the starting strength value for ith team

s = team.strengths[,3] # the stenghts for the t teams

h = 2           # starting value for h
                # this may change of subsequent running of Bradley.Terry 

games = read.csv('nba.csv', header = F, sep = ',') # the games in the schedule  

g = length(games[,1]) # the number of games in this particular schedule

MC =  1E02/((.6^g) * exp( -sum(s)))
                 # multiplier constant to make the value of the posterior readable
                 # g*g is to make this constant grow as the number of games grows
                 # g*g might not be enough

opt = optomize.par() # find the optimal solution
   # opt will be a list containing
   #   s     -- a vector containing the estimated strengths of the t-teams
   #   h     -- the estimated home court advantage
   #   post  -- the value of the posterior
   #   inter -- how many interations did it take to get a solution

team.strengths[,3] = opt$s

write.csv(team.strengths, file = 'team.strengthsBT.out.csv')

prob.homecourt.victory = opt$h/(opt$h+1)

home = list(homecourt=opt$h,posterior=opt$post, 
            n.of.iterations=opt$n.iter,probHV=prob.homecourt.victory)

write.csv(home,file='home.out.csv')

# end Bradley.Terry
# note t needs to be changed for for different schedules