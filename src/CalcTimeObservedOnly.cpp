#include <Rcpp.h>
using namespace Rcpp;
#define SecsPerYear 31557600

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


//' Multiply a number by two
//' 
//' @param x A single integer.
// [[Rcpp::export]]
NumericVector CalcTimeObservedOnlyCpp(NumericVector ID, NumericVector Abs_dates, NumericVector Rating, NumericVector labels, double end_date) {
  int nlabel = labels.size();
  int ndata = ID.size();
  NumericVector TT(nlabel);
  for(int ii=0; ii<ndata-1; ++ii) {// don't consider last row (and indexing starts from 0)
    if(ID(ii)==ID(ii+1)){ /* ie if two rows are same asset*/
      TT(Rating(ii)-1) +=  (Abs_dates(ii+1) -Abs_dates(ii))/SecsPerYear;
    } 
  }
  
  return TT;
}
