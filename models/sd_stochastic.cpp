#include <Rcpp.h>
using namespace Rcpp;

// helper: integer binomial draw
inline int rbinom_int(int n, double p) {
  if (n <= 0 || p <= 0.0) return 0;
  if (p >= 1.0) return n;
  return R::rbinom(n, p);
}

// [[Rcpp::export]]
DataFrame sd_stochastic(
  int init_population,
  int A,                  // mean discharges per day
  double r,               // spontaneous colonisation rate (constant across phases)
  double baseline_beta,
  double baseline_lambda,
  double outbreak_beta,
  double outbreak_lambda,
  double control_beta,
  int outbreak_start,
  int control_start,
  int outbreak_end,
  int control_end,
  int n_days
) {
  // initial state
  int S = std::round(init_population * (1.0 - baseline_lambda));
  int I = std::round(init_population * baseline_lambda);
  
  // output storage
  IntegerVector days(n_days + 1);
  IntegerVector S_out(n_days + 1);
  IntegerVector I_out(n_days + 1);
  IntegerVector comm_cases(n_days + 1);
  IntegerVector hosp_cases(n_days + 1);
  IntegerVector dis_S_out(n_days + 1);
  IntegerVector dis_I_out(n_days + 1);
  
  // initial record
  days[0]        = 0;
  S_out[0]       = S;
  I_out[0]       = I;
  comm_cases[0]  = 0;
  hosp_cases[0]  = 0;
  dis_S_out[0]   = 0;
  dis_I_out[0]   = 0;
  
  // simulation loop
  for (int t = 1; t <= n_days; ++t) {
    // choose parameter regime using four time points
    // beta varies across baseline/outbreak/control; p only varies across baseline/outbreak (not affected by control)
    double beta, lambda;
    // community colonisation rate (lambda): only changes during outbreak window
    if (t > outbreak_start && t < outbreak_end) {
      lambda = outbreak_lambda;
    } else {
      lambda = baseline_lambda;
    }
    // transmission coefficient (beta): changes during outbreak and control windows
    if (t > control_start && t < control_end) {
      beta = control_beta;
    } else if (t > outbreak_start && t < outbreak_end) {
      beta = outbreak_beta;
    } else {
      beta = baseline_beta;
    }
    
    // --- 1. Discharges ---
    int total_leave = R::rpois(A);
    int total_pop   = S + I;
    if (total_pop == 0) total_pop = 1;
    
    int dis_S = std::round(total_leave * ((double) S / total_pop));
    int dis_I = total_leave - dis_S;
    
    // update S, I after discharges
    S -= dis_S;
    I -= dis_I;
    
    // --- 2. In-hospital infections ---
    int inf_S   = rbinom_int(S, beta);              // transmission infections
    int S_after_inf = S - inf_S;
    int spont_S = rbinom_int(S_after_inf, r);       // spontaneous infections
    int new_infections = inf_S + spont_S;
    
    // --- 3. Admissions (equal to discharges) ---
  int adm_S = rbinom_int(total_leave, 1.0 - lambda);
  int adm_I = total_leave - adm_S;
    
    // --- 4. Update totals ---
    S = S - new_infections + adm_S;
    I = I + new_infections + adm_I;
    
    // --- 5. Record daily outputs ---
    days[t]        = t;
    S_out[t]       = S;
    I_out[t]       = I;
    comm_cases[t]  = adm_I;             // community-acquired
    hosp_cases[t]  = new_infections;    // hospital-acquired (transmission + spontaneous)
    dis_S_out[t]   = dis_S;
    dis_I_out[t]   = dis_I;
  }
  
  return DataFrame::create(
    _["day"]        = days,
    _["S"]          = S_out,
    _["I"]          = I_out,
    _["comm_cases"] = comm_cases,
    _["hosp_cases"] = hosp_cases,
    _["dis_S"]      = dis_S_out,
    _["dis_I"]      = dis_I_out
  );
}
