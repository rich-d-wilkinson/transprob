#include <Rcpp.h>
#include <iostream>
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
NumericMatrix CalcTransCpp(NumericVector ID, NumericVector Rating, NumericVector labels) {
int nlabel = labels.size();
int ndata = ID.size();
  NumericMatrix NN(nlabel, nlabel);
  for(int ii=0; ii<ndata-1; ++ii) {// don't consider last row (and indexing starts from 0)
    if(ID(ii)==ID(ii+1)){ /* ie if two rows are same asset*/
        ++NN(Rating(ii)-1,Rating(ii+1)-1);  // increment relevant element - note indexing begins at 0 in C++
    }
  }
  return NN;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//
/*** R
CalcTransCpp( c(rep(1,5), rep(2,5),rep(3,5)), c(rep(c(1,2),5), rep(3,5)), c(1,2,3))
*/