#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double getpi() {
  return M_PI; 
  }

// [[Rcpp::export]]
double loglikelihood(NumericVector theta, NumericVector X, NumericVector Y){
  double a = theta[0];
  double b = theta[1];
  double sd = theta[2];
  int n = X.length();
  
  NumericVector yy(n);
  for(int i=0; i < n; i++){
    yy[i] = a*X[i] + b ;
  }

  NumericVector singleLikelihood(n);
  double num;
  double den;
  
  for (int i=0; i < n; i++){
    num = exp(-pow((Y[i]-yy[i]), 2.0)/(2*sd*sd));
    den = sd*sqrt(2*getpi());
    singleLikelihood[i] = log(num/den);
    //singleLikelihood[i] = R::dnorm(Y[i], yy[i], sd, true);
  }
  
  double sumAll = sum(singleLikelihood);
  return (sumAll);
}

// [[Rcpp::export]]
double logPriori(NumericVector theta){
  double a = theta[0];
  double b = theta[1];
  double sd = theta[2];
  
  double a_ = R::dunif(a, 0, 50, true);
  double b_ = R::dnorm(b, 0, 5, true);
  double sd_ = R::dunif(sd, 0, 50,true);
  double aux = a_+b_+sd_;
  return(aux);
}

// [[Rcpp::export]]
double logPosteriori(NumericVector theta, NumericVector X, NumericVector Y){
  double aux1 = loglikelihood(theta, X, Y);
  double aux2 = logPriori(theta);
  aux2 = aux2 + aux1;
  return(aux2);
}

// [[Rcpp::export]]
NumericVector proposal(NumericVector theta){
  double a = theta[0];
  double b = theta[1];
  double sd = theta[2];
  
  double p1 = R::rnorm(a,  0.1);
  double p2 = R::rnorm(b,  0.5);
  double p3 = R::rnorm(sd, 0.3);
  
  NumericVector aux(3);
  aux[0] = p1;
  aux[1] = p2;
  aux[2] = p3;
  return (aux);
}

// [[Rcpp::export]]
NumericMatrix runMCMC(NumericVector x, NumericVector y, NumericVector startValue, int iterations){
  NumericMatrix chain(iterations+1, 3);
  for(int i=0; i < startValue.length(); i++){
    chain(0,i) = startValue[i];
  }
  
  NumericVector prop(3);
  NumericVector aux(3);
  double probab;
  for(int i=0; i < iterations; i++){
    for(int j=0; j < 3; j++){            //auxiliar, vector parametros
      aux[j] = chain(i, j);
    }
    prop = proposal(aux);
    probab = exp(logPosteriori(prop, x, y) - logPosteriori(aux, x, y));
    if(R::runif(0,1) < probab){
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = prop[j];
      }
    }else{
      for(int j=0; j < startValue.length(); j++){
        chain(i+1, j) = chain(i,j);
       }
      }
    }
    return(chain);
}

 /*** R
  theta <- c(1.0,2.,3.)
  x <- seq(-10,10, length.out = 100)
  y <- x + 5 + rnorm(100, mean=0, sd=3)
  loglikelihood(theta, x, y)
  logPriori(theta)
  logPosteriori(theta, x, y)
  proposal(theta)
  runMCMC(x, y, theta, 1000)
  */
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 