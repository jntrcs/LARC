#include <Rcpp.h>
#include <vector>
#include<cmath>
using namespace Rcpp;

//This is the C++ functions for calculating the modes of the posterior distributions
//run Rcpp::sourceCpp("cppFiles.cpp") before using these functions

#include <cmath>
//[[Rcpp::export]]
double logBTDensity(NumericVector strengths, IntegerMatrix wins, NumericVector winsTotal, NumericVector magnificationfactor=1)
{
  long double ans=1;

  for (int i = 0; i<strengths.size(); ++i)
  {
    
    
    ans =ans + (winsTotal.at(i)+1)*log(strengths.at(i)) - strengths.at(i);
    // if (pi>tooBig)
    //{
    //  pi = pi/downer;
    //  pipi=pipi * downer;
    //}
    if (i<strengths.size())
    {
      for (int j = i+1; j<strengths.size(); ++j){
        if (wins.row(j)[i]+wins.row(i)[j]!=0)
        {
          ans =(ans+(wins.row(j)[i]+wins.row(i)[j])*log(1/(strengths.at(i)+strengths.at(j))));
        }
      }
    }
  }
  
  return ans;
}

//[[Rcpp::export]]
double BTDensity(NumericVector strengths, IntegerMatrix wins, NumericVector winsTotal, NumericVector magnificationfactor=1)
{
  double mf=magnificationfactor.at(0);
  long double ans=1;
  for (int i = 0; i<strengths.size(); ++i)
  {
    
    
    ans = ans * pow(strengths.at(i), winsTotal.at(i)+1)*exp(-strengths.at(i));
    // if (pi>tooBig)
    //{
    //  pi = pi/downer;
    //  pipi=pipi * downer;
    //}
    if (i<strengths.size())
    {
      for (int j = i+1; j<strengths.size(); ++j){
        if (wins.row(j)[i]+wins.row(i)[j]!=0)
        {
          ans =ans*(1/pow(strengths.at(i)+strengths.at(j), wins.row(j)[i]+wins.row(i)[j]))*mf;
        }
      }
    }
  }
  
  return ans;
}


//[[Rcpp::export]]
double logTMDensity(NumericVector strengths, IntegerMatrix wins, NumericVector winsTotal=0, NumericVector magFac=1)
{
  //Normal cdf code from https://www.johndcook.com/blog/cpp_phi/, part of public domain
  double logoneoversqrt = -0.91893853320467274;
  double a1 =  0.254829592;
  double a2 = -0.284496736;
  double a3 =  1.421413741;
  double a4 = -1.453152027;
  double a5 =  1.061405429;
  double p  =  0.3275911; 
  double sumStrengths=0;
  int n = strengths.size();
  for (int i = 0; i<n; ++i)
  {
    sumStrengths+=pow(strengths.at(i),2)/2;
  }
  double sumCDFs = 0;
  for (int i =0; i<n; ++i)
  {
    for (int j=0; j<n; ++j)
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
        sumCDFs+=wins.row(i)[j]*log(phi);
      }
    }
  }
  return n*logoneoversqrt-sumStrengths+sumCDFs;
}  
//[[Rcpp::export]]
double TMDensity(NumericVector strengths, IntegerMatrix wins, NumericVector winsTotal=0, NumericVector magFac=1)
{
  //Normal cdf code from https://www.johndcook.com/blog/cpp_phi/, part of public domain
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