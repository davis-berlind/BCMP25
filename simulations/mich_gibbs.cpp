#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <vector>
#include <iomanip>

using namespace Rcpp;

int sample_int(NumericVector probs) {
  double u = R::runif(0.0, 1.0);

  double cum = 0.0;
  for (int i = 0; i < probs.size(); i++) {
    cum += probs[i];
    if (u <= cum)
      return i;
  }

  // In case of numerical roundoff
  return probs.size();
}

List smcp_sample(
    NumericMatrix QTr,
    NumericMatrix mean_weights,
    NumericMatrix sd_weights,
    NumericMatrix sandwich_weights,
    NumericVector log_prob
) {
  int T = QTr.nrow();
  int d = QTr.ncol();
  NumericVector QTr_sum (d, 0.0);
  NumericVector log_pi_bar (T+1, 0.0);
  NumericMatrix QTb_bar (T+1, d);

  log_pi_bar = clone(log_prob);
  double log_pi_max = log_prob[T];

  for (int t = T - 1; t >= 0; t--) {
    for (int i = 0; i < d; i++) {
      QTr_sum[i] += QTr(t, i);
      log_pi_bar[t] += sandwich_weights(t, i) * QTr_sum[i] * QTr_sum[i];
      QTb_bar(t,i) = mean_weights(t, i) * QTr_sum[i];
    }
    log_pi_max = std::max(log_pi_max, log_pi_bar[t]);
  }

  double tot = 0.0;
  NumericVector pi_bar (T+1, 0.0);
  for (int t = 0; t < T+1; t++) {
    log_pi_bar[t] -= log_pi_max;
    pi_bar[t] = std::exp(log_pi_bar[t]);
    tot += pi_bar[t];
  }
  pi_bar = pi_bar / tot;

  int tau = sample_int(pi_bar);

  NumericVector QTb (d, 0.0);
  if (tau < T) {
    for (int i = 0; i < d; i++) {
      double z = R::rnorm(0.0, 1.0);
      QTb[i] = QTb_bar(tau, i) + sd_weights(tau, i) * z;
    }
  }

  List sample = List::create(
    _["tau"] = tau,
    _["QTb"] = QTb
  );

  return sample;
}

// [[Rcpp::export]]
List mich_gibbs_cpp(
    NumericMatrix y,
    int L,
    NumericVector lambda,
    NumericMatrix Q,
    int B,
    int thin,
    int burnin,
    bool verbose,
    NumericVector log_prob,
    NumericMatrix mean_weights,
    NumericMatrix sandwich_weights,
    NumericMatrix sd_weights
) {
  int d = y.ncol();
  int T = y.nrow();
  int total_iter = B * thin + burnin;

  NumericMatrix r = clone(y);
  NumericMatrix QTr (T, d);

  for (int t = 0; t < T; t++) {
    for (int i = 0; i < d; i++) {
      for (int j = 0; j < d; j++) {
        QTr(t, i) += r(t, j) * Q(j, i);
      }
    }
  }

  IntegerMatrix tau_post (B, L);
  NumericMatrix mu_post (B, d * T);
  LogicalMatrix active(B, T+1);
  List b_post (L);
  for (int l = 0; l < L; l++) {
    b_post[l] = NumericMatrix(B, d);
  }

  NumericMatrix QTb (d, L);
  IntegerVector tau (L);

  int sample_dex = 0;
  for (int iter = 1; iter <= total_iter; iter++) {
    for (int l = 0; l < L; l++) {
      int tau_l = tau[l];
      for (int t = tau_l; t < T; t++) {
        for (int i = 0; i < d; i++) {
          QTr(t, i) += QTb(i, l);
        }
      }

      List fit = smcp_sample(
        QTr, mean_weights, sd_weights, sandwich_weights, log_prob
      );

      tau_l = as<int>(fit["tau"]);
      tau[l] = tau_l;
      QTb(_, l) = as<NumericVector>(fit["QTb"]);

      if (iter > burnin && iter % thin == 0) {
        tau_post(sample_dex, l) = tau_l + 1;
        active(sample_dex, tau_l) = true;
        SEXP b_post_l_sexp = b_post[l];
        NumericMatrix b_post_l(b_post_l_sexp);   // zero-copy proxy
        for (int i = 0; i < d; i++) {
          for (int j = 0; j < d; j++) {
            b_post_l(sample_dex, i) += Q(i, j) * QTb(j, l);
          }
          for (int t = tau_l; t < T; t++) {
            mu_post(sample_dex, t + i * T) += b_post_l(sample_dex, i);
          }
        }
        if (l == L-1) sample_dex++;
      }

      for (int t = tau_l; t < T; t++) {
        for (int i = 0; i < d; i++) {
          QTr(t, i) -= QTb(i, l);
        }
      }
    }

    // std::vector<int> ord(L);
    // std::iota(ord.begin(), ord.end(), 0);
    //
    // std::sort(
    //   ord.begin(), ord.end(),
    //   [&](int a, int b) {
    //     return tau[a] < tau[b];
    //   }
    // );
    //
    // IntegerVector tau_ordered(L);
    // NumericMatrix QTb_ordered (d, L);
    //
    // for (int l = 0; l < L; l++) {
    //   tau_ordered[l] = tau[ord[l]];
    //   QTb_ordered(_,l) = QTb(_,ord[l]);
    // }
    //
    // QTb = clone(QTb_ordered);
    // tau = clone(tau_ordered);
    //
    // for (int l = 0; l < L; l++) {
    //   tau_post(iter, l) = tau[l];
    //   SEXP b_post_l_sexp = b_post[l];
    //   NumericMatrix b_post_l(b_post_l_sexp);   // zero-copy proxy
    //   for (int i = 0; i < d; i++) {
    //     for (int j = 0; j < d; j++) {
    //       b_post_l(iter, i) += Q(i, j) * QTb(j, l);
    //     }
    //   }
    // }

    const int barWidth = 50;

    if (verbose && (iter % 100 == 0 || iter == total_iter)) {
      double progress = double(iter) / total_iter;
      int pos = int(barWidth * progress);

      Rcout << "\r[";
      for (int i = 0; i < barWidth; ++i) {
        if (i < pos)
          Rcout << "=";
        else if (i == pos)
          Rcout << ">";
        else
          Rcout << " ";
      }

      Rcout << "] "
            << std::setw(6) << std::fixed << std::setprecision(1)
            << 100 * progress << "%" << std::flush;
    }

    if (verbose && iter == total_iter) Rcout << std::endl;
  }

  List result = List::create(
    _["tau"] = tau_post,
    _["b"] = b_post,
    _["mu"] = mu_post,
    _["active"] = active
  );
  return result;
}
