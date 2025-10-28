source(here::here("scripts", "helpers", "simdesign_random_helpers.R"))

sim_days <- (365 * 3) + 30
# sim_hours <- sim_days * 24

nl <- nl(nlversion = nl_version,
         nlpath = netlogo_path,
         modelpath = model_path,
         jvmmem = 1024)

nl@experiment <- experiment(expname = "baseline_rate",
                            outpath = out_path,
                            repetition = 1,
                            tickmetrics = "true",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = sim_days,
                            metrics = c("total-colonised",
                                        "total-patients-admitted",
                                        "total-hospital-infections",
                                        "current-community-infections",
                                        "current-hospital-infections",
                                        "current-inpatients",
                                        "current-colonised"),
                            variables = list("community-colonisation-rate" = list(min=0.01, max=0.1, qfun='qunif'),
                                             "toilet-contamination-effect" = list(min=0.01, max=0.99, qfun='qunif'),
                                             "toilet-cleaning-effect" = list(min=0.01, max=0.99, qfun='qunif'),
                                             "toilet-frequenting-rate" = list(min=0.5, max=5, qfun='qunif'),
                                             "antibiotic-effect" = list(min=1.31, max=1.87, qfun='qunif'),
                                             "toilet-cleaning-rate" = list(min=0.5, max=3, qfun='qunif'),
                                             "random-colonisation" = list(min=2.8, max=12.1, qfun='qunif'),
                                             "side-room-delay-mean" = list(min=0.01, max=7, qfun='qunif')
                            ),
                            constants = list("wards-total" = wards_total,
                                             "outbreak?" = FALSE,
                                             "infection-control?" = FALSE,
                                             "bedspaces-per-ward" = bedspaces_per_ward,
                                             "antibiotic-prescription-rate" = antibiotic_prescription_rate,
                                             "admission-days" = admission_days,
                                             "bay-proportion" = bay_proportion))

nl@simdesign <- simdesign_fn(nl,
                                 samples = calibration_samples,
                                 nseeds = calibration_seeds,
                                 precision = 3)
# Note: simdesign_fn draws independent random samples from the prior distributions
# defined in nl@experiment@variables, unlike simdesign_fn which uses Latin Hypercube Sampling.
# Seeds are generated using nlrx:::util_generate_seeds for reproducibility.
# To control randomness, set a seed before calling this function.

plan(list(sequential, future_plan), workers = num_workers)

if (run_sims) {
  results_baseline <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(results_baseline, file.path(out_path, "results_baseline.rds"))
}

results_baseline <- read_rds(file.path(out_path, "results_baseline.rds"))

results_baseline_bak <- results_baseline
results_baseline <- results_baseline %>% 
  filter(`[step]` > 31)

results_baseline <- results_baseline %>% 
  group_by(siminputrow, `random-seed`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_baseline_rates <- results_baseline %>% 
  mutate(year = year(date_sim), month = str_pad(month(date_sim), 2, pad = "0")) %>% 
  group_by(`random-seed`, siminputrow, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_baseline_summary <- results_baseline %>% distinct(`random-seed`, siminputrow, .keep_all = TRUE) %>% 
  # mutate(siminputrow = siminputrow) %>% 
  right_join(results_baseline_rates, by=c("siminputrow", "random-seed"))

params_names_pre_outbreak <- names(nl@experiment@variables)

salgado <- read_csv(file.path(data_path, "salgado_et_al.csv")) %>%
  mutate(x = seq(ymd('2002-01-01'), by = '1 month', length.out=nrow(.)))

pre_outbreak_data <- salgado %>% 
  filter(x < ymd("2004-10-01"))

pre_outbreak_sims <- results_baseline_summary %>% 
  dplyr::select(all_of(params_names_pre_outbreak), `random-seed`, siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim < ymd("2004-10-01")) %>% 
  dplyr::select(!date_sim) %>% 
  group_by(siminputrow, month, year) %>%
  summarise(rate = mean(rate)) %>%
  pivot_wider(names_from = c(year, month), values_from = rate,
              names_prefix = "rate_")

pre_outbreak_sims <- results_baseline_summary %>% 
  ungroup %>% 
  dplyr::select(siminputrow, all_of(params_names_pre_outbreak)) %>%
  distinct(siminputrow, .keep_all = TRUE) %>%
  right_join(pre_outbreak_sims)

sumstats <- pre_outbreak_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_")) %>% 
  dplyr::select(sort(names(.)))

abc_params <- abc(target = pre_outbreak_data$rates,
                  param = pre_outbreak_sims[params_names_pre_outbreak],
                  sumstat = sumstats,
                  tol = abc_tol,
                  method="rejection")

# plot
long_retained_ss <- abc_params$ss %>% 
  as_tibble %>% 
  #slice_sample(n=10) %>% 
  mutate(i = seq(nrow(.)),
         type = "sim") %>% 
  pivot_longer(starts_with("rate_"),
               names_to = "x",
               values_to = "rates") %>% 
  mutate(x = str_remove(x, "rate_")) %>% 
  mutate(x = str_replace(x, "_", "-")) %>% 
  mutate(x = paste0(x, "-1")) %>% 
  mutate(x = ymd(x))

long_observed_ss <- pre_outbreak_data %>% 
  mutate(type = "obs")

sim_mean <- long_retained_ss %>% 
  group_by(x) %>% 
  summarise(m = mean(rates),
            sd = sd(rates))

abc_params$unadj.values %>% 
  as_tibble %>% 
  pivot_longer(cols=everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_density() +
  facet_wrap(~ name, scales='free')

ggplot(long_retained_ss) + geom_line(aes(x = x, y = rates, group = i, color = type)) +
  geom_line(data = long_observed_ss, aes(x = x, y = rates, color = type))

ggplot(long_observed_ss, aes(x = x, y = rates)) + geom_line(color = 'red') +
  geom_line(data = sim_mean, aes(x = x, y = m)) +
  geom_line(data = sim_mean, aes(x = x, y = m + sd), linetype='dashed') +
  geom_line(data = sim_mean, aes(x = x, y = m - sd), linetype='dashed')
