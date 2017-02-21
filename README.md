"# LARC" 

This read me will provide a basic walkthrough to some of the content in this repository. 

MasterFunctionFile.RData contains all the functions that were used in the analysis, the function definitions are available in files in the repository. Loading MasterFunctionFile will prepare your workspace to run various analysis within the paper

The command Rcpp::sourceCpp("cppFiles.cpp") which is found at the top of various files that require it will load the necessary c++ compiled functions into your workspace, which allow the computation of posterior modes much more quickly than the native R versions

The files Stripped2015FootballData.RData and Stripped2016FootballData.RData contain the raw data scraped from masseyratings.com for football outcomes, as well as a list including various analysis elements. The following will explain how to navigate the list:

1. The top level list corresponds to each time period that the data was analyzed for. e.g. stripped2016data[[1]] contains all analysis on week one of the data. It has the following elements:
  a. The first element (stripped2016data[[1]][[1]]) in each week list is the configuration matrix necessary to compute posterior densities on the data. It contains a matrix of which team played which team and who    won.
  b. The second element (stripped2016data[[1]][[2]]) contains the Bradley-Terry rankings ending at that weekof the season.
  c. The third element contains the Thurstone-Mosteller Rankings for the same time period
  d. The fourth element contains analysis of predictions using strengths from the previous week compared with actual outcomes. Therefore, the first week (stripped2016data[[1]][[4]])is NULL, but the following weeks      have information in the form of a list with the following elements:
    1. All games played that week with predictions from both methods, actual outcomes, and summary statistics.
    2. A count of which method predicted closer to the actual outcome
    3. A penalty metric 
    4. The average difference in prediction for the two methods
    5. The number of games in the week
    6. The brier scores for each method
    7. The log scores for each method
  e. The fifth element lists the start and end dates for the time period covered