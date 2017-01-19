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

#include <cmath>


//[[Rcpp::export]]
double BTDensity(NumericVector strengths, IntegerMatrix wins, NumericVector winsTotal, NumericVector magnificationfactor=1)
{
  double mf=magnificationfactor.at(0);
  double pi = 1;
  long double pipi=1;
  double tooBig = pow(10, 200);
  double downer=pow(10, 15);
  //NumericVector w;
  for (int i = 0; i<strengths.size(); ++i)
  {
    //int sum=0;
    //for (int j = 0; j<wins.ncol(); ++j)
    //{
    //  sum+=wins.row(i)[j]; //I think we could save serious computation time by not recomputing this constantly
    //}
    //w.push_back(sum);
   
    pi = pi * pow(strengths.at(i), winsTotal.at(i)+1)*exp(-strengths.at(i));
    if (pi>tooBig)
    {
      pi = pi/downer;
      pipi=pipi * downer;
    }
    if (i<strengths.size())
    {
      for (int j = i+1; j<strengths.size(); ++j){
        if (wins.row(j)[i]+wins.row(i)[j]!=0)
        {
          pipi =pipi*(1/pow(strengths.at(i)+strengths.at(j), wins.row(j)[i]+wins.row(i)[j]))*mf;
        }
        }
    }
  }

  return pi*pipi;
}

//[[Rcpp::export]]
double TMDensity(NumericVector strengths, IntegerMatrix wins, NumericVector winsTotal=0, NumericVector magFac=1)
{
  //I need to cite the author of the phi function which I am copying in here. 
  long double prior = 1;
  double oneoversqrt = 0.39894228040143267793994605993;
  double a1 =  0.254829592;
  double a2 = -0.284496736;
  double a3 =  1.421413741;
  double a4 = -1.453152027;
  double a5 =  1.061405429;
  double p  =  0.3275911;
  double mf = magFac.at(0);
  for (int i = 0; i<strengths.size(); i++)
  {
    prior=prior*mf*oneoversqrt*exp(-1* pow(strengths.at(i),2) / 2);
  }
  long double cond = 1;
  for (int i =0; i<strengths.size(); ++i)
  {
    for (int j = 0; j<strengths.size(); ++j)
    {
      if (wins.row(i)[j]!=0)
      {
        int sign = 1;
        double x=strengths.at(i)-strengths(j);
        if (x < 0)
          sign = -1;
        x = fabs(x)/sqrt(2.0);
        
        // A&S formula 7.1.26
        double t = 1.0/(1.0 + p*x);
        double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x);
        double phi= 0.5*(1.0 + sign*y);
        cond = cond * mf * pow(phi,wins.row(i)[j]);
      }
    }
  }
  return prior*cond;
}


