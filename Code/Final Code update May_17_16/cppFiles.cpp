#include <Rcpp.h>
#include <vector>
#include<cmath>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}
//[[Rcpp::export]]
double BTDensity(IntegerMatrix wins, NumericVector strengths)
{
  double pi = 1;
  double pipi=1;
  NumericVector w;
  for (int i = 0; i<strengths.size(); ++i)
  {
    int sum=0;
    for (int j = 0; j<wins.ncol(); ++j)
    {
      sum+=wins.row(i)[j]; //I think we could save serious computation time by not recomputing this constantly
    }
    w.push_back(sum);
    pi = pi * pow(strengths.at(i), w.at(i)+1);
    if (i<strengths.size())
    {
      for (int j = i+1; j<strengths.size(); ++j){
        //std::cout<<"I: "<<i<<" J: "<<j<<std::endl;
        if (wins.row(j)[i]+wins.row(i)[j]!=0)
        {
          pipi =pipi*(1/pow(strengths.at(i)+strengths.at(j), wins.row(j)[i]+wins.row(i)[j]));
        }
        }
    }
  }
  int strengthsum=0;
  for (int j = 0; j<strengths.size(); ++j)
  {
    strengthsum+=strengths.at(j); 
  }
  
  return exp(-1*strengthsum)*pi*pipi;
}

/*BradleyTerryLARC <- function(strengths,wins,magnificationfactor=1) {
 PI <- 1
PIPI <- 1
W <- vector()
for (i in 1:length(strengths)) {
W[i] <- sum(wins[i,])
PI <- PI*strengths[i]^(W[i]+1)
for (j in (i+1):length(strengths)) {
if (j < length(strengths)+1) {
if ((wins[i,j]+wins[j,i])>0)
PIPI <- PIPI*(1/(strengths[i]+strengths[j])^(wins[i,j]+wins[j,i]))*magnificationfactor
}
}
}
return(exp(-sum(strengths))*PI*PIPI)
}*/


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(99)
BTDensity(Employeedf$WinsVersus, Employeedf$Strength)
*/
