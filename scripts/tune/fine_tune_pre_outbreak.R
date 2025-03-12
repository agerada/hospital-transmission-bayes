sim_days <- (365 * 3) + 30

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogo_path,
         modelpath = model_path,
         jvmmem = 1024)

pre_outbreak_priors <- abc_params$unadj.values
pre_outbreak_priors_means <- apply(pre_outbreak_priors, 2, mean)
pre_outbreak_priors_sd <- apply(pre_outbreak_priors, 2, sd)
pre_outbreak_priors_lower_sd <- pre_outbreak_priors_means - pre_outbreak_priors_sd
pre_outbreak_priors_upper_sd <- pre_outbreak_priors_means + pre_outbreak_priors_sd

pre_outbreak_variables <- apply(pre_outbreak_priors, 2, \(x) {
  list(min=unname(quantile(x, 0.25)),
       max=unname(quantile(x, 0.75)),
       qfun='qunif')
})

nl@experiment <- experiment(expname = "fine_tune",
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
                            variables = pre_outbreak_variables,
                            constants = list("wards-total" = wards_total,
                                             "bedspaces-per-ward" = bedspaces_per_ward,
                                             "antibiotic-prescription-rate" = antibiotic_prescription_rate, # Vesporten 2018
                                             "outbreak?" = FALSE,
                                             "infection-control?" = FALSE,
                                             "admission-days" = admission_days,
                                             "bay-proportion" = bay_proportion))

nl@simdesign <- simdesign_lhs(nl,
                              samples = calibration_samples,
                              nseeds = calibration_seeds,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  results_baseline_fine_tune <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(results_baseline_fine_tune, file.path(out_path, "results_baseline_fine_tune.rds"))
}

results_baseline_fine_tune <- read_rds(file.path(out_path, "results_baseline_fine_tune.rds"))

results_baseline_fine_tune_bak <- results_baseline_fine_tune
results_baseline_fine_tune <- results_baseline_fine_tune %>% 
  filter(`[step]` > 31)

results_baseline_fine_tune <- results_baseline_fine_tune %>% 
  group_by(siminputrow, `random-seed`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_baseline_rates <- results_baseline_fine_tune %>% 
  mutate(year = year(date_sim), month = str_pad(month(date_sim), 2, pad = "0")) %>% 
  group_by(`random-seed`, siminputrow, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_baseline_summary <- results_baseline_fine_tune %>% distinct(siminputrow, `random-seed`, .keep_all = TRUE) %>% 
  # mutate(siminputrow = siminputrow) %>% 
  right_join(results_baseline_rates, by=c("siminputrow", "random-seed"))

params_names_pre_outbreak <- names(nl@experiment@variables)

salgado <- read_csv(file.path(data_path, "salgado_et_al.csv")) %>%
  mutate(x = seq(ymd('2002-01-01'), by = '1 month', length.out=nrow(.)))

pre_outbreak_data <- salgado %>% 
  filter(x < ymd("2004-10-01"))

pre_outbreak_sims <- results_baseline_summary %>% 
  dplyr::select(all_of(params_names_pre_outbreak), siminputrow, month, year, rate) %>% 
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
