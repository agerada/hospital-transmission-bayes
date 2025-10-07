sim_days <- (365 * 7) + 30

# outbreak_control_model <- "salgado_v0.3.nlogo"

nl <- nl(nlversion = nl_version,
         nlpath = netlogo_path,
         modelpath = model_path,
         jvmmem = 1024)

consts <- list("wards-total" = wards_total,
               "bedspaces-per-ward" = bedspaces_per_ward,
               "antibiotic-prescription-rate" = 0.319, # Vesporten 2018
               "admission-days" = admission_days,
               "bay-proportion" = bay_proportion)

# use abc mean parameter estimates as constants for baseline
consts <- c(consts, apply(abc_params$unadj.values, MARGIN = 2, mean))

# set outbreak and control
consts <- c(consts, "outbreak?" = TRUE, "infection-control?" = TRUE)

# Set control measures start, which in Salgado was third week of November.
# Calculate how many ticks into simulation this would be
eicm_start <- as.numeric(difftime(ymd('2004-11-15'), ymd('2002-01-01'), units = 'days'))
consts <- c(consts, "control-start" = eicm_start)

# in Salgado, the outbreak starts in October 2004, calculate how many ticks/days
# into simulation this is, then offset by 90 days in the search field
outbreak_start <- as.numeric(difftime(ymd('2004-10-01'), ymd('2002-01-01'), units = 'days'))
outbreak_variables <- list("outbreak-start" = list(min=outbreak_start - 90,
                                                   max=outbreak_start + 90,
                                                   qfun='qunif'),
                           "o-toilet-frequenting-rate" = list(min=consts[["toilet-frequenting-rate"]],
                                                              max=4,
                                                              qfun='qunif'),
                           "o-toilet-contamination-effect" = list(min=consts[["toilet-contamination-effect"]],
                                                                  max=0.99,
                                                                  qfun='qunif'),
                           "o-toilet-cleaning-effect" = list(min=0.01,
                                                             max=consts[["toilet-cleaning-effect"]],
                                                             qfun='qunif'),
                           "o-toilet-cleaning-rate" = list(min=0.5,
                                                           max=consts[["toilet-cleaning-rate"]],
                                                           qfun='qunif'),
                           "o-community-colonisation-rate" = list(min=consts[["community-colonisation-rate"]],
                                                                  max=consts[["community-colonisation-rate"]] + 0.5 * consts[["community-colonisation-rate"]],
                                                                  qfun='qunif'),
                           "o-antibiotic-prescription-rate" = list(min=antibiotic_prescription_rate,
                                                                   max=antibiotic_prescription_rate + 0.5 * antibiotic_prescription_rate,
                                                                   qfun='qunif'),
                           "o-proportion-redistributed" = list(min=0.01,
                                                               max=consts[["proportion-redistributed"]],
                                                               qfun='qunif'),
                           "c-toilet-cleaning-effect" = list(min=consts[["toilet-cleaning-effect"]],
                                                             max=0.99,
                                                             qfun='qunif'),
                           "c-toilet-cleaning-rate" = list(min=consts[["toilet-cleaning-rate"]],
                                                           max=consts[["toilet-cleaning-rate"]] + 0.5 * consts[["toilet-cleaning-rate"]],
                                                           qfun='qunif'),
                           "c-antibiotic-prescription-rate" = list(min=antibiotic_prescription_rate - 0.5 * antibiotic_prescription_rate,
                                                                   max=antibiotic_prescription_rate,
                                                                   qfun='qunif'),
                           "c-proportion-redistributed" = list(min=consts[["proportion-redistributed"]],
                                                               max=0.99,
                                                               qfun='qunif')
)


nl@experiment <- experiment(expname = "outbreak_control",
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
                            variables = outbreak_variables,
                            constants = consts)

nl@simdesign <- simdesign_lhs(nl,
                              samples = calibration_samples,
                              nseeds = calibration_seeds,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  results_outbreak_control <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(results_outbreak_control, file.path(out_path, "results_outbreak_control.rds"))
}

results_outbreak_control <- read_rds(file.path(out_path, "results_outbreak_control.rds"))

results_outbreak_control_bak <- results_outbreak_control
results_outbreak_control <- results_outbreak_control %>% 
  filter(`[step]` > 31)

results_outbreak_control <- results_outbreak_control %>% 
  group_by(siminputrow, `random-seed`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_outbreak_control_rates <- results_outbreak_control %>% 
  mutate(year = year(date_sim), month = str_pad(month(date_sim), 2, pad = "0")) %>% 
  group_by(siminputrow, `random-seed`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_outbreak_control_summary <- results_outbreak_control %>% distinct(siminputrow, `random-seed`, .keep_all = TRUE) %>% 
  # mutate(siminputrow = siminputrow) %>% 
  right_join(results_outbreak_control_rates, by=c("siminputrow", "random-seed"))

params_names_outbreak_control <- names(nl@experiment@variables)

# salgado <- read_csv("data/salgado_et_al.csv") %>%
#   mutate(x = seq(ymd('2002-01-01'), by = '1 month', length.out=nrow(.)))

# pre_outbreak_data <- salgado %>% 
#   filter(x < ymd("2004-10-01"))

outbreak_control_sims <- results_outbreak_control_summary %>% 
  dplyr::select(all_of(params_names_outbreak_control), `random-seed`, siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim <= max(salgado$x)) %>% 
  dplyr::select(!date_sim) %>% 
  group_by(siminputrow, month, year) %>%
  summarise(rate = mean(rate)) %>%
  pivot_wider(names_from = c(year, month), values_from = rate,
              names_prefix = "rate_")

outbreak_control_sims <- results_outbreak_control_summary %>%
  ungroup %>% 
  dplyr::select(siminputrow, all_of(params_names_outbreak_control)) %>%
  distinct(siminputrow, .keep_all = TRUE) %>%
  right_join(outbreak_control_sims)

sumstats <- outbreak_control_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_")) %>% 
  dplyr::select(sort(names(.)))

abc_params_outbreak_control <-abc(target = salgado$rates,
                                  param = outbreak_control_sims[params_names_outbreak_control],
                                  sumstat = sumstats,
                                  tol = abc_tol,
                                  method="rejection")

# plot
long_retained_ss <- abc_params_outbreak_control$ss %>% 
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

long_observed_ss <- salgado %>% 
  mutate(type = "obs")

sim_mean <- long_retained_ss %>% 
  group_by(x) %>% 
  summarise(m = mean(rates),
            sd = sd(rates))

abc_params_outbreak_control$unadj.values %>% 
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
