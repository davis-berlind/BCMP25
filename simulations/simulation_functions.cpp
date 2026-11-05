#include <Rcpp.h>
using namespace Rcpp;

//' Hausdorff Statistic
//'
//' @description
//' Calculates Hausdorff distance between two sets of estimated change-points 
//'
//' @param x,y Integer vectors. Locations of the change-points.
//'
//' @return A scalar. The Hausdorff distance between `x` and `y`.
//'
// [[Rcpp::export]]
double hausdorff(NumericVector x, NumericVector y) {
  double max_dist = 0.0;
  for (int i = 0; i < x.length(); i++) {
    double min_dist = R_PosInf;
    for (int j = 0; j < y.length(); j++) {
      double dist = std::abs(x[i] - y[j]);
      if (dist < min_dist) min_dist = dist;
    }
    if (min_dist > max_dist) max_dist = min_dist;
  }
  
  for (int j = 0; j < y.length(); j++) {
    double min_dist = R_PosInf;
    for (int i = 0; i < x.length(); i++) {
      double dist = std::abs(x[i] - y[j]);
      if (dist < min_dist) min_dist = dist;
    }
    if (min_dist > max_dist) max_dist = min_dist;
  }
  
  return(max_dist);
}

//' Hausdorff Statistic for Sorted Sets
//'
//' @description
//' Calculates Hausdorff distance between two sorted sets of change-points 
//'
//' @param x,y Integer vectors. Sorted locations of the change-points.
//'
//' @return A scalar. The Hausdorff distance between `x` and `y`.
//'
// [[Rcpp::export]]
double hausdorff_sorted(NumericVector x, NumericVector y) {
  double max_dist = 0.0;
  
  int j = 0;
  for (int i = 0; i < x.size(); i++) {
    while (j + 1 < y.size() &&
           std::abs(x[i] - y[j+1]) < std::abs(x[i] - y[j])) {
      j++;
    }
    max_dist = std::max(max_dist, std::abs(x[i] - y[j]));
  }
  
  int i = 0;
  for (int j = 0; j < y.size(); j++) {
    while (i + 1 < x.size() &&
           std::abs(y[j] - x[i+1]) < std::abs(y[j] - x[i])) {
      i++;
    }
    max_dist = std::max(max_dist, std::abs(y[j] - x[i]));
  }
  
  return max_dist;
}

//' Hausdorff Statistic of Matrix
//'
//' @description
//' Calculates the Hausdorff distance between each row of two matrices
//'
//' @param X,Y Integer matrices. Locations of the change-points.
//' @param sorted Logical. Flag to use `hausdorff` vs `hausdorff_sorted`.
//'
//' @return A matrix. The Hausdorff distance between each row of `X` and `Y`.
//'
// [[Rcpp::export]]
NumericMatrix matrix_hausdorff(NumericMatrix X, NumericMatrix Y, bool sorted) {
  int N = X.nrow();
  int M = Y.nrow();
  NumericMatrix haus(N, M);
  for (int i=0; i < N; i++) {
    for (int j=0; j < M; j++) {
      if (sorted) haus(i,j) = hausdorff_sorted(X(i,_), Y(j,_));
      else haus(i,j) = hausdorff(X(i,_), Y(j,_));
    }
  }
  return(haus);
}