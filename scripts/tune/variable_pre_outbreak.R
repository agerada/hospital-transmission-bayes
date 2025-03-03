sim_days <- (365 * 7) + 30

# outbreak_control_model <- "salgado.nlogo"

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogo_path,
         modelpath = model_path,
         jvmmem = 1024)

consts <- list("wards-total" = wards_total,
               "bedspaces-per-ward" = bedspaces_per_ward,
               "antibiotic-prescription-rate" = antibiotic_prescription_rate, # Vesporten 2018
               "admission-days" = admission_days,
               "bay-proportion" = bay_proportion)

# set outbreak and control
consts <- c(consts, "outbreak?" = TRUE, "infection-control?" = TRUE)

# Set control measures start, which in Salgado was third week of November.
# Calculate how many ticks into simulation this would be
eicm_start <- as.numeric(difftime(ymd('2004-11-15'), ymd('2002-01-01'), units = 'days'))
consts <- c(consts, "control-start" = eicm_start)

# in Salgado, the outbreak starts in October 2004, calculate how many ticks/days
# into simulation this is, then offset by 90 days in the search field
outbreak_start <- as.numeric(difftime(ymd('2004-10-01'), ymd('2002-01-01'), units = 'days'))

# also need to calculate the end date of the outbreak
outbreak_end <- as.numeric(difftime(ymd('2005-05-31'), ymd('2002-01-01'), units = 'days'))

# set an end to enhanced control measures, assume these were stepped down 
# three months after end of outbreak
control_end <- outbreak_end + 90

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

outbreak_variables <- list("outbreak-start" = list(min=outbreak_start - 90,
                                                   max=outbreak_start + 90,
                                                   qfun='qunif'),
                           "outbreak-end" = list(min=outbreak_end - 90,
                                                 max=outbreak_end + 90,
                                                 qfun='qunif'),
                           "control-end" = list(min=control_end - 30, 
                                                max=control_end + 30,
                                                qfun='qunif'),
                           "o-toilet-frequenting-rate" = list(min=pre_outbreak_variables[["toilet-frequenting-rate"]]$min,
                                                              max=pre_outbreak_priors_means[["toilet-frequenting-rate"]] + 0.5 * pre_outbreak_priors_means[["toilet-frequenting-rate"]],
                                                              qfun='qunif'),
                           "o-toilet-contamination-effect" = list(min=pre_outbreak_variables[["toilet-contamination-effect"]]$min,
                                                                  max=min(0.99, pre_outbreak_priors_means[["toilet-contamination-effect"]] + 0.5 * pre_outbreak_priors_means[["toilet-contamination-effect"]]),
                                                                  qfun='qunif'),
                           "o-toilet-cleaning-effect" = list(min=max(0.01, pre_outbreak_priors_means[["toilet-cleaning-effect"]] - 0.5 * pre_outbreak_priors_means[["toilet-cleaning-effect"]]),
                                                             max=pre_outbreak_variables[["toilet-cleaning-effect"]]$max,
                                                             qfun='qunif'),
                           "o-toilet-cleaning-rate" = list(min=0.5,
                                                           max=pre_outbreak_variables[["toilet-cleaning-rate"]]$max,
                                                           qfun='qunif'),
                           "o-community-colonisation-rate" = list(min=pre_outbreak_variables[["community-colonisation-rate"]]$min,
                                                                  max=pre_outbreak_priors_means[["community-colonisation-rate"]] + 0.5 * pre_outbreak_priors_means[["community-colonisation-rate"]],
                                                                  qfun='qunif'),
                           "o-antibiotic-prescription-rate" = list(min=antibiotic_prescription_rate,
                                                                   max=min(0.99, antibiotic_prescription_rate + 0.5 * antibiotic_prescription_rate), # also from Vesporten 2018
                                                                   qfun='qunif'),
                           "o-proportion-redistributed" = list(min=0.01,
                                                               max=pre_outbreak_variables[["proportion-redistributed"]]$max,
                                                               qfun='qunif'),
                           "c-toilet-cleaning-effect" = list(min=pre_outbreak_variables[["toilet-cleaning-effect"]]$min,
                                                             max=0.99,
                                                             qfun='qunif'),
                           "c-toilet-cleaning-rate" = list(min=pre_outbreak_variables[["toilet-cleaning-rate"]]$min,
                                                           max=pre_outbreak_priors_means[["toilet-cleaning-rate"]] + 0.5 * pre_outbreak_priors_means[["toilet-cleaning-rate"]],
                                                           qfun='qunif'),
                           "c-antibiotic-prescription-rate" = list(min=max(0.01, antibiotic_prescription_rate - 0.5 * antibiotic_prescription_rate),
                                                                   max=antibiotic_prescription_rate,
                                                                   qfun='qunif'),
                           "c-proportion-redistributed" = list(min=pre_outbreak_variables[["proportion-redistributed"]]$min,
                                                               max=0.99,
                                                               qfun='qunif')
)

outbreak_variables <- c(outbreak_variables, pre_outbreak_variables)

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
  results_outbreak_control_pre_included <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(results_outbreak_control_pre_included, file.path(out_path, "results_outbreak_control_pre_included.rds"))
}

results_outbreak_control_pre_included <- read_rds(file.path(out_path, "results_outbreak_control_pre_included.rds"))

results_outbreak_control_pre_included_bak <- results_outbreak_control_pre_included
results_outbreak_control_pre_included <- results_outbreak_control_pre_included %>% 
  filter(`[step]` > 31)

results_outbreak_control_pre_included <- results_outbreak_control_pre_included %>% 
  group_by(siminputrow, `random-seed`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_outbreak_control_pre_included_rates <- results_outbreak_control_pre_included %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(siminputrow, `random-seed`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_outbreak_control_pre_included_summary <- results_outbreak_control_pre_included %>% distinct(`random-seed`, siminputrow, .keep_all = TRUE) %>% 
  # mutate(siminputrow = siminputrow) %>% 
  right_join(results_outbreak_control_pre_included_rates, by=c("siminputrow", "random-seed"))

params_names_outbreak_control <- names(nl@experiment@variables)

# keep simulated parameters that fit with outbreak generation
results_outbreak_control_pre_included_summary <- results_outbreak_control_pre_included_summary %>% 
  dplyr::filter(`o-toilet-contamination-effect` >= `toilet-contamination-effect` &
                  `o-toilet-frequenting-rate` >= `toilet-frequenting-rate` &
                  `o-community-colonisation-rate` >= `community-colonisation-rate`)

outbreak_control_sims <- results_outbreak_control_pre_included_summary %>% 
  dplyr::select(all_of(params_names_outbreak_control), `random-seed`, siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim <= max(salgado$x)) %>% 
  dplyr::select(!date_sim) %>% 
  group_by(siminputrow, month, year) %>%
  summarise(rate = mean(rate)) %>%
  pivot_wider(names_from = c(year, month), values_from = rate,
              names_prefix = "rate_")

outbreak_control_sims <- results_outbreak_control_pre_included_summary %>%
  ungroup %>% 
  dplyr::select(siminputrow, all_of(params_names_outbreak_control)) %>%
  distinct(siminputrow, .keep_all = TRUE) %>%
  right_join(outbreak_control_sims)

sumstats <- outbreak_control_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_"))

abc_params_outbreak_control <-abc(target = salgado$rates,
                                  param = outbreak_control_sims[params_names_outbreak_control],
                                  sumstat = sumstats,
                                  tol = abc_tol,
                                  method="rejection",
                                  transf = "none")

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

# sim_mean <- long_retained_ss %>% 
#   group_by(x) %>% 
#   summarise(m = mean(rates),
#             sd = sd(rates))

sim_mean <- long_retained_ss %>% 
  group_by(x) %>% 
  summarise(m = median(rates),
            q_low = quantile(rates, 0.25),
            q_high = quantile(rates, 0.75))

abc_params_outbreak_control$unadj.values %>% 
  as_tibble %>% 
  pivot_longer(cols=everything()) %>% 
  ggplot(aes(x = value)) + 
  geom_density() +
  facet_wrap(~ name, scales='free')

ggplot(long_retained_ss) + geom_line(aes(x = x, y = rates, group = i, color = type)) +
  geom_line(data = long_observed_ss, aes(x = x, y = rates, color = type))

# ggplot(long_observed_ss, aes(x = x, y = rates)) + geom_line(color = 'red') +
#   geom_line(data = sim_mean, aes(x = x, y = m)) +
#   geom_line(data = sim_mean, aes(x = x, y = m + sd), linetype='dashed') +
#   geom_line(data = sim_mean, aes(x = x, y = m - sd), linetype='dashed')

ggplot(long_observed_ss, aes(x = x, y = rates)) + geom_line(color = 'red') +
  geom_line(data = sim_mean, aes(x = x, y = m)) +
  geom_line(data = sim_mean, aes(x = x, y = q_high), linetype='dashed') +
  geom_line(data = sim_mean, aes(x = x, y = q_low), linetype='dashed')

# save params
abc_params_outbreak_control$unadj.values %>% 
  t %>% 
  as.data.frame %>% 
  write.table(file.path(out_path, "abc_params.csv"),
              sep = ",",
              row.names = TRUE,
              col.names = FALSE)
