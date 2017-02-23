"# LARC" 

This read me will provide a basic walkthrough to some of the content in this repository. 

MasterFunctionFile.RData contains all the functions that were used in the analysis, the function definitions are available in files in the repository. Loading MasterFunctionFile will prepare your workspace to run various analysis within the paper

The command Rcpp::sourceCpp("cppFiles.cpp") which is found at the top of various files that require it will load the necessary c++ compiled functions into your workspace, which allow the computation of posterior modes much more quickly than the native R versions

The files Stripped2015FootballData.RData and Stripped2016FootballData.RData contain the raw data scraped from masseyratings.com for football outcomes, as well as a list including various analysis elements. The following will explain how to navigate the list:

1. The top level list corresponds to each time period that the data was analyzed for. e.g. stripped2016data[[1]] contains all analysis on week one of the data. It has the following elements:
  1. The first element (stripped2016data[[1]][[1]]) in each week list is the configuration matrix necessary to compute posterior densities on the data. It contains a matrix of which team played which team and who    won.
  2. The second element (stripped2016data[[1]][[2]]) contains the Bradley-Terry rankings ending at that weekof the season.
  3. The third element contains the Thurstone-Mosteller Rankings for the same time period
  4. The fourth element contains analysis of predictions using strengths from the previous week compared with actual outcomes. Therefore, the first week (stripped2016data[[1]][[4]])is NULL, but the following weeks      have information in the form of a list with the following elements:
    1. All games played that week with predictions from both methods, actual outcomes, and summary statistics.
    2. A count of which method predicted closer to the actual outcome
    3. A penalty metric (which we didn't use in favor of brier scores)
    4. The average difference in prediction for the two methods
    5. The number of games in the week
    6. The brier scores for each method
    7. The log scores for each method (did not use in favor of brier scores)
  5. The fifth element lists the start and end dates for the time period covered
  
2015/2016FootballData.RData contains the same data but with games where FBS teams played teams outside the FBS.

LARC.Rank() is the main computational function for ranking the teams. To test it on your own results open the Template.csv sheet and edit it following the pattern shown in the document with your data. When finished, save, and run the first three lines in EasyUseExcel.R to see the rankings.

dataconfigure() creates the matrix that can be used by LARC.Rank. DataConfigure.R specifies the format of the data frame it accepts.


####RECREATING THE SIMULATION
To run a miniature version of the simulation, run SampleSimulation.R. To see the full, parallelizable version that was run on a supercomputer, view SupercomputerSim.R

Every season simulated produces a .RData file. SimDataLoader.R loads each file one at a time and conglomerates the data into useful groups in memory.

<<<<<<< HEAD
SimulationSetUp contains the main logic to simulate a season. Simulate1 conducts the bulk of the work by creating the teams, assigning them strengths, generating a schedule, and then simulating the results of the games on the schedule. It then uses LARC.Rank to find strength estimates for every week, and then computes useful statistic on the data and returns the results in a list.
=======

>>>>>>> aaac420ab3138d211b62dacbb3c6ce3d5adefdc6
  