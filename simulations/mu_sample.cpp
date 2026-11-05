#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <vector>
#include <iomanip>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix mu_sample_cpp(
    IntegerMatrix tau,
    List b_post,
    NumericMatrix Q,
    NumericMatrix sd_weights
) {
  int B = tau.nrow();
  int L = tau.ncol();
  int d = sd_weights.ncol();
  int T = sd_weights.nrow();

  NumericMatrix mu_post (B, d * T);

  for (int i=0; i < B; i++) {
    for (int l=0; l < L; l++) {
      int tau_l = tau(i,l);
      NumericMatrix b_post_l = b_post[l];
      NumericVector b = b_post_l(tau_l,_);
      if (tau_l < T) {
        for (int j = 0; j < d; j++) {
          double z = R::rnorm(0.0, sd_weights(tau_l, j));
          for (int k = 0; k < d; k++) {
            b[k] += Q(k, j) * z;
          }
        }
      }
      for (int j = 0; j < d; j++) {
        for (int t = tau_l; t < T; t++) {
          mu_post(i, t + j * T) += b[j];
        }
      }
    }
  }

  return mu_post;
}

