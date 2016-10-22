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


//[[Rcpp::export]]
double BTDensity(NumericVector strengths, IntegerMatrix wins, NumericVector magFac=1)
{
  double mf=magFac.at(0);
  double pi = 1;
  long double pipi=1;
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
          pipi =pipi*(1/pow(strengths.at(i)+strengths.at(j), wins.row(j)[i]+wins.row(i)[j]))*mf;
        }
        }
    }
  }
  int strengthsum=0;
  for (int j = 0; j<strengths.size(); ++j)
  {
    strengthsum+=strengths.at(j); 
  }
  //std::cout<<"Pi: "<<pi<<" Pipi: "<<pipi<<" Strengthsummed: "<< strengthsum<<std::endl; 
  //std::cout<<exp(-1*strengthsum)*pi*pipi;
  return exp(-1*strengthsum)*pi*pipi;
}

//[[Rcpp::export]]
double TMDensity(NumericVector strengths, IntegerMatrix wins, NumericVector magFac=1)
{
  long double prior = 1;
  double oneoversqrt = 0.39894228040143267793994605993;
  for (int i = 0; i<strengths.size(); i++)
  {
    prior=prior*magFac.at(0)*oneoversqrt*exp(-1* pow(strengths.at(i),2) / 2);
  }
  long double cond = 1;
  for (int i =0; i<strengths.size(), ++i)
  {
    for (int j = 0; j<strengths.size(); ++j)
    {
      if (wins.row(i)[j]!=0)
        cond = cond * magFac.at(0) * pow(  ,wins.row(i)[j])
    }
  }
}


#include <cmath>

double phi(double x)
{
  // constants
  double a1 =  0.254829592;
  double a2 = -0.284496736;
  double a3 =  1.421413741;
  double a4 = -1.453152027;
  double a5 =  1.061405429;
  double p  =  0.3275911;
  
  // Save the sign of x
  int sign = 1;
  if (x < 0)
    sign = -1;
  x = fabs(x)/sqrt(2.0);
  
  // A&S formula 7.1.26
  double t = 1.0/(1.0 + p*x);
  double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);
  
  return 0.5*(1.0 + sign*y);
}

/*ThurstoneMostellerLARC <- function(strengths,wins,magnificationfactor=1) {
# First we compute the Prior
  prior <- 1
  for (i in 1: length(strengths)) { prior = prior * dnorm( strengths[i])*magnificationfactor}
# Now we compute the conditional
  cond <- 1
  for (i in 1:length(strengths)) {
    for (j in 1:length(strengths)) {
      if (wins[i,j]!=0)
        cond <- cond*pnorm(strengths[i]-strengths[j])^wins[i,j]*magnificationfactor
    }
  }
# Now put the two together 
  post = prior * cond
    return(post)
}*/

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
BTDensity(all2015data[[16]][[1]]$Strength,all2015data[[16]][[1]]$WinsVersus, 2 )
*/
