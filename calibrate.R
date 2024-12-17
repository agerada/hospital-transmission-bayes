library(nlrx)
library(future)
library(tidyverse)
library(lubridate)
library(abc)
library(progressr)
handlers("cli")
set.seed(42)

netlogo_path <- "/Applications/NetLogo 6.2.2/"
model_path <- "../salgado_v0.42_day_time.nlogo"
out_path <- "../out/"
data_path <- "../data/"

# run_sims <- FALSE

calibration_samples <- 100
calibration_seeds <- 3
abc_tol <- 0.05

antibiotic_prescription_rate = 0.499 # Vesporten 2018
admission_days <- 4.6
bay_proportion <- 0.6

##============== pre-outbreak =============##

sim_days <- (365 * 3) + 30
# sim_hours <- sim_days * 24

nl <- nl(nlversion = "6.2.2",
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
                                             "proportion-redistributed" = list(min=0.01, max=0.99, qfun='qunif')
                                             ),
                            constants = list("wards-total" = wards_total,
                                             "outbreak?" = FALSE,
                                             "infection-control?" = FALSE,
                                             "bedspaces-per-ward" = bedspaces_per_ward,
                                             "antibiotic-prescription-rate" = antibiotic_prescription_rate,
                                             "admission-days" = admission_days,
                                             "bay-proportion" = bay_proportion))

nl@simdesign <- simdesign_lhs(nl,
                              samples = calibration_samples,
                              nseeds = calibration_seeds,
                              precision = 3)

plan(list(sequential, multisession))

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
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
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
  dplyr::select(starts_with("rate_"))

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

##============== outbreak and control ===============##

sim_days <- (365 * 7) + 30

# outbreak_control_model <- "salgado_v0.3.nlogo"

nl <- nl(nlversion = "6.2.2",
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
                                                              max=8,
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
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
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
  dplyr::select(starts_with("rate_"))

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


##=========== fine tune pre-outbreak ===================================##

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
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
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
  dplyr::select(starts_with("rate_"))

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

##=========== use variables for pre-outbreak params as well ============##

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
                                                              max=8,
                                                              qfun='qunif'),
                           "o-toilet-contamination-effect" = list(min=pre_outbreak_variables[["toilet-contamination-effect"]]$min,
                                                                  max=0.99,
                                                                  qfun='qunif'),
                           "o-toilet-cleaning-effect" = list(min=0.01,
                                                             max=pre_outbreak_variables[["toilet-cleaning-effect"]]$max,
                                                             qfun='qunif'),
                           "o-toilet-cleaning-rate" = list(min=0.5,
                                                           max=pre_outbreak_variables[["toilet-cleaning-rate"]]$max,
                                                           qfun='qunif'),
                           "o-community-colonisation-rate" = list(min=pre_outbreak_variables[["community-colonisation-rate"]]$min,
                                                                  max=pre_outbreak_variables[["community-colonisation-rate"]]$max + 0.5 * pre_outbreak_variables[["community-colonisation-rate"]]$max,
                                                                  qfun='qunif'),
                           "o-antibiotic-prescription-rate" = list(min=antibiotic_prescription_rate,
                                                                   max=antibiotic_prescription_rate + 0.5 * antibiotic_prescription_rate, # also from Vesporten 2018
                                                                   qfun='qunif'),
                           "o-proportion-redistributed" = list(min=0.01,
                                                               max=pre_outbreak_variables[["proportion-redistributed"]]$max,
                                                               qfun='qunif'),
                           "c-toilet-cleaning-effect" = list(min=pre_outbreak_variables[["toilet-cleaning-effect"]]$min,
                                                             max=0.99,
                                                             qfun='qunif'),
                           "c-toilet-cleaning-rate" = list(min=pre_outbreak_variables[["toilet-cleaning-rate"]]$min,
                                                           max=pre_outbreak_variables[["toilet-cleaning-rate"]]$max + 2,
                                                           qfun='qunif'),
                           "c-antibiotic-prescription-rate" = list(min=antibiotic_prescription_rate - 0.5 * antibiotic_prescription_rate,
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

##================= simulate using consts only ==================##

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

estimated_consts_mean <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, mean)

estimated_consts_sd <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, sd)

estimated_consts <- map2(estimated_consts_mean, estimated_consts_sd, \(x, y) {
  list(min = x - y,
       max = x + y,
       qfun = 'qunif')
})

estimated_consts_fixed <- c(estimated_consts_mean, consts)

nl@experiment <- experiment(expname = "consts_sim",
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
                            constants = consts, 
                            variables = estimated_consts)

nl@simdesign <- simdesign_lhs(nl,
                              samples = 20,
                              nseeds = 10,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  consts_sim <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(consts_sim, file.path(out_path, "consts_sim.rds"))
}

consts_sim <- read_rds(file.path(out_path, "consts_sim.rds"))

consts_sim_bak <- consts_sim
consts_sim <- consts_sim %>% 
  filter(`[step]` > 31)

consts_sim <- consts_sim %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

consts_sim_rates <- consts_sim %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `siminputrow`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))


consts_sim_rates %>% 
  pivot_wider(names_from = date_sim, values_from = rate) %>% 
  #slice_sample(n=10) %>% 
  pivot_longer(cols = !c(`random-seed`, `siminputrow`), names_to = "date_sim", values_to = "rate") %>% 
  mutate(date_sim = ymd(date_sim)) %>% 
  group_by(siminputrow, date_sim) %>%
  summarise(rate = mean(rate)) %>%
  ggplot(aes(x = date_sim,
             y = rate,
             color = factor(`siminputrow`))) +
  geom_line() +
  geom_line(data = long_observed_ss, aes(x = x, y = rates), color = "black") +
  guides(color="none")

# pre-outbreak data
median(pre_outbreak_data$rates)
quantile(pre_outbreak_data$rates, 0.25)
quantile(pre_outbreak_data$rates, 0.75)

# outbreak data
outbreak_start_date <- ymd("2004-10-01") 
outbreak_end_date <- ymd("2005-05-31")
salgado %>% 
  dplyr::filter(x >= outbreak_start_date & x <= outbreak_end_date) %>% 
  summarise(across(rates, list(median = median, q_low = \(x) quantile(x, 0.25), q_high = \(x) quantile(x, 0.75), min = min, max = max)))

# simulated data
pre_outbreak_sims_rates <- consts_sim_rates %>% 
  dplyr::filter(date_sim < outbreak_start_date) %>% 
  summarise(across(rate, list(median = median, q_low = \(x) quantile(x, 0.25), q_high = \(x) quantile(x, 0.75))))

outbreak_sims_rates <- consts_sim_rates %>% 
  dplyr::filter(date_sim >= outbreak_start_date & date_sim <= outbreak_end_date) %>% 
  summarise(across(rate, list(median = median, q_low = \(x) quantile(x, 0.25), q_high = \(x) quantile(x, 0.75), min = min, max = max)))

# save.image(file.path(out_path, "calibrate.RData"))

###========== single outbreak only simulation =========###

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
consts <- c(consts, "outbreak?" = TRUE, "infection-control?" = FALSE)

estimated_consts_mean <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, mean)

estimated_consts_sd <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, sd)

estimated_consts <- map2(estimated_consts_mean, estimated_consts_sd, \(x, y) {
  list(min = x - y,
       max = x + y,
       qfun = 'qunif')
})

estimated_consts_fixed <- c(estimated_consts_mean, consts)

nl@experiment <- experiment(expname = "single_outbreak_sim",
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
                            constants = consts, 
                            variables = estimated_consts)

nl@simdesign <- simdesign_lhs(nl,
                              samples = 1,
                              nseeds = 10,
                              precision = 3)

plan(multisession)

if (run_sims) {
  single_outbreak_sim <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(single_outbreak_sim, file.path(out_path, "single_outbreak_sim.rds"))
}

single_outbreak_sim <- read_rds(file.path(out_path, "single_outbreak_sim.rds"))

single_outbreak_sim_bak <- single_outbreak_sim
single_outbreak_sim <- single_outbreak_sim %>% 
  filter(`[step]` > 31)

single_outbreak_sim <- single_outbreak_sim %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

single_outbreak_sim_rates <- single_outbreak_sim %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `siminputrow`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))


single_outbreak_sim_rates %>% 
  pivot_wider(names_from = date_sim, values_from = rate) %>% 
  #slice_sample(n=10) %>% 
  pivot_longer(cols = !c(`random-seed`, `siminputrow`), names_to = "date_sim", values_to = "rate") %>% 
  mutate(date_sim = ymd(date_sim)) %>% 
  group_by(`random-seed`, date_sim) %>%
  summarise(rate = mean(rate)) %>%
  ggplot(aes(x = date_sim,
             y = rate,
             color = factor(`random-seed`))) +
  geom_line() +
  geom_line(data = long_observed_ss, aes(x = x, y = rates), color = "black") +
  guides(color="none")

##================= sensitivity analysis ==================##

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

estimated_consts_mean <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, mean)

estimated_consts_mean <- estimated_consts_mean[names(estimated_consts_mean) != "c-antibiotic-prescription-rate"]

nl@experiment <- experiment(expname = "sens_analysis",
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
                            constants = c(consts, estimated_consts_mean), 
                            variables = list("c-antibiotic-prescription-rate" = list(min=0.1,
                                                                                  max=0.7,
                                                                                  qfun='qunif')
                                             )
                            )

nl@simdesign <- simdesign_lhs(nl,
                              samples = 20,
                              nseeds = 10,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  sens_abx_prescription <- progressr::with_progress(
    run_nl_all(nl))
    write_rds(sens_abx_prescription, file.path(out_path, "sens_abx_prescription.rds"))
}

sens_abx_prescription <- read_rds(file.path(out_path, "sens_abx_prescription.rds"))

sens_abx_prescription_bak <- sens_abx_prescription

sens_abx_prescription <- sens_abx_prescription %>% 
  filter(`[step]` > 31)

sens_abx_prescription <- sens_abx_prescription %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

sens_abx_prescription_rates <- sens_abx_prescription %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-antibiotic-prescription-rate`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

time_triggers <- estimated_consts_mean[names(estimated_consts_mean) %in%
                                         c("outbreak-start",
                                           "outbreak-end",
                                           "control-end")]
filter_from <- round(min(time_triggers))
filter_to <- round(max(time_triggers))

sens_abx_prescription_outbreak_rates <- sens_abx_prescription %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-antibiotic-prescription-rate`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

sens_abx_prescription_outbreak_rates %>% 
  group_by(`c-antibiotic-prescription-rate`, `random-seed`) %>% 
  summarise(rate = mean(rate)) %>%
  group_by(`c-antibiotic-prescription-rate`) %>%
  summarise(mean_rate = mean(rate),
            sd_rate = sd(rate)) %>% 
  ggplot(aes(x = `c-antibiotic-prescription-rate`, y = mean_rate)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = lower), linetype = "dashed")

sens_abx_prescription_outbreak_cases <- sens_abx_prescription %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>%
  group_by(`random-seed`, `c-antibiotic-prescription-rate`) %>% 
  mutate(`outbreak-cases` = `total-hospital-infections` - min(`total-hospital-infections`)) %>% 
  summarise(`outbreak-cases` = max(`outbreak-cases`))

sens_abx_prescription_outbreak_cases %>% 
  group_by(`c-antibiotic-prescription-rate`) %>% 
  summarise(mean_cases = mean(`outbreak-cases`),
            sd_cases = sd(`outbreak-cases`)) %>% 
  ggplot(aes(x = `c-antibiotic-prescription-rate`, y = mean_cases)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = lower), linetype = "dashed")

#####============= sens proportion redistributed

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

estimated_consts_mean <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, mean)

estimated_consts_mean <- estimated_consts_mean[names(estimated_consts_mean) != "c-proportion-redistributed"]

nl@experiment <- experiment(expname = "sens_analysis",
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
                            constants = c(consts, estimated_consts_mean), 
                            variables = list("c-proportion-redistributed" = list(min=0.01,
                                                                                     max=0.99,
                                                                                     qfun='qunif')
                            )
)

nl@simdesign <- simdesign_lhs(nl,
                              samples = 20,
                              nseeds = 10,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  sens_prop_redist <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(sens_prop_redist, file.path(out_path, "sens_prop_redist.rds"))
}

sens_prop_redist <- read_rds(file.path(out_path, "sens_prop_redist.rds"))

sens_prop_redist_bak <- sens_prop_redist

sens_prop_redist <- sens_prop_redist %>% 
  filter(`[step]` > 31)

sens_prop_redist <- sens_prop_redist %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

sens_prop_redist_rates <- sens_prop_redist %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-proportion-redistributed`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

time_triggers <- estimated_consts_mean[names(estimated_consts_mean) %in%
                                         c("outbreak-start",
                                           "outbreak-end",
                                           "control-end")]
filter_from <- round(min(time_triggers))
filter_to <- round(max(time_triggers))

sens_prop_redist_outbreak_rates <- sens_prop_redist %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-proportion-redistributed`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

sens_prop_redist_outbreak_rates %>% 
  group_by(`c-proportion-redistributed`, `random-seed`) %>% 
  summarise(rate = mean(rate)) %>%
  group_by(`c-proportion-redistributed`) %>%
  summarise(mean_rate = mean(rate),
            sd_rate = sd(rate)) %>% 
  ggplot(aes(x = `c-proportion-redistributed`, y = mean_rate)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = lower), linetype = "dashed")

sens_prop_redist_outbreak_cases <- sens_prop_redist %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>%
  group_by(`random-seed`, `c-proportion-redistributed`) %>% 
  mutate(`outbreak-cases` = `total-hospital-infections` - min(`total-hospital-infections`)) %>% 
  summarise(`outbreak-cases` = max(`outbreak-cases`))

sens_prop_redist_outbreak_cases %>% 
  group_by(`c-proportion-redistributed`) %>% 
  summarise(mean_cases = mean(`outbreak-cases`),
            sd_cases = sd(`outbreak-cases`)) %>% 
  ggplot(aes(x = `c-proportion-redistributed`, y = mean_cases)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = lower), linetype = "dashed")

list(abx_prescription_rate = sens_abx_prescription_outbreak_cases,
     proportion_redistributed = sens_prop_redist_outbreak_cases) %>%
  bind_rows() %>% 
  pivot_longer(cols = starts_with("c-"),
               names_to = "param",
               values_drop_na = TRUE) %>% 
  group_by(param, value) %>% 
  summarise(mean_cases = mean(`outbreak-cases`),
            sd_cases = sd(`outbreak-cases`)) %>% 
  ggplot(aes(x = value, y = mean_cases)) + 
  geom_line() + 
  facet_wrap(~ param)

#####============= sens toilet cleaning rate ==================##

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

estimated_consts_mean <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, mean)

estimated_consts_mean <- estimated_consts_mean[names(estimated_consts_mean) != "c-toilet-cleaning-rate"]

nl@experiment <- experiment(expname = "sens_analysis",
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
                            constants = c(consts, estimated_consts_mean), 
                            variables = list("c-toilet-cleaning-rate" = list(min=0.01,
                                                                                 max=0.99,
                                                                                 qfun='qunif')
                            )
)

nl@simdesign <- simdesign_lhs(nl,
                              samples = 20,
                              nseeds = 10,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  sens_prop_redist <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(sens_prop_redist, file.path(out_path, "sens_prop_redist.rds"))
}

sens_prop_redist <- read_rds(file.path(out_path, "sens_prop_redist.rds"))

sens_prop_redist_bak <- sens_prop_redist

sens_prop_redist <- sens_prop_redist %>% 
  filter(`[step]` > 31)

sens_prop_redist <- sens_prop_redist %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

sens_prop_redist_rates <- sens_prop_redist %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-toilet-cleaning-rate`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

time_triggers <- estimated_consts_mean[names(estimated_consts_mean) %in%
                                         c("outbreak-start",
                                           "outbreak-end",
                                           "control-end")]
filter_from <- round(min(time_triggers))
filter_to <- round(max(time_triggers))

sens_prop_redist_outbreak_rates <- sens_prop_redist %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-toilet-cleaning-rate`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

sens_prop_redist_outbreak_rates %>% 
  group_by(`c-toilet-cleaning-rate`, `random-seed`) %>% 
  summarise(rate = mean(rate)) %>%
  group_by(`c-toilet-cleaning-rate`) %>%
  summarise(mean_rate = mean(rate),
            sd_rate = sd(rate)) %>% 
  ggplot(aes(x = `c-toilet-cleaning-rate`, y = mean_rate)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = lower), linetype = "dashed")

sens_prop_redist_outbreak_cases <- sens_prop_redist %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>%
  group_by(`random-seed`, `c-toilet-cleaning-rate`) %>% 
  mutate(`outbreak-cases` = `total-hospital-infections` - min(`total-hospital-infections`)) %>% 
  summarise(`outbreak-cases` = max(`outbreak-cases`))

sens_prop_redist_outbreak_cases %>% 
  group_by(`c-toilet-cleaning-rate`) %>% 
  summarise(mean_cases = mean(`outbreak-cases`),
            sd_cases = sd(`outbreak-cases`)) %>% 
  ggplot(aes(x = `c-toilet-cleaning-rate`, y = mean_cases)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = lower), linetype = "dashed")

list(abx_prescription_rate = sens_abx_prescription_outbreak_cases,
     proportion_redistributed = sens_prop_redist_outbreak_cases) %>%
  bind_rows() %>% 
  pivot_longer(cols = starts_with("c-"),
               names_to = "param",
               values_drop_na = TRUE) %>% 
  group_by(param, value) %>% 
  summarise(mean_cases = mean(`outbreak-cases`),
            sd_cases = sd(`outbreak-cases`)) %>% 
  ggplot(aes(x = value, y = mean_cases)) + 
  geom_line() + 
  facet_wrap(~ param)

####================ sensitivity toilet cleaning effect ==============####

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

estimated_consts_mean <- abc_params_outbreak_control$unadj.values %>%
  apply(., 2, mean)

estimated_consts_mean <- estimated_consts_mean[names(estimated_consts_mean) != "c-toilet-cleaning-effect"]

nl@experiment <- experiment(expname = "sens_analysis",
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
                            constants = c(consts, estimated_consts_mean), 
                            variables = list("c-toilet-cleaning-effect" = list(min=0.01,
                                                                                 max=0.99,
                                                                                 qfun='qunif')
                            )
)

nl@simdesign <- simdesign_lhs(nl,
                              samples = 20,
                              nseeds = 10,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  sens_prop_redist <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(sens_prop_redist, file.path(out_path, "sens_prop_redist.rds"))
}

sens_prop_redist <- read_rds(file.path(out_path, "sens_prop_redist.rds"))

sens_prop_redist_bak <- sens_prop_redist

sens_prop_redist <- sens_prop_redist %>% 
  filter(`[step]` > 31)

sens_prop_redist <- sens_prop_redist %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

sens_prop_redist_rates <- sens_prop_redist %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-toilet-cleaning-effect`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

time_triggers <- estimated_consts_mean[names(estimated_consts_mean) %in%
                                         c("outbreak-start",
                                           "outbreak-end",
                                           "control-end")]
filter_from <- round(min(time_triggers))
filter_to <- round(max(time_triggers))

sens_prop_redist_outbreak_rates <- sens_prop_redist %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-toilet-cleaning-effect`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

sens_prop_redist_outbreak_rates %>% 
  group_by(`c-toilet-cleaning-effect`, `random-seed`) %>% 
  summarise(rate = mean(rate)) %>%
  group_by(`c-toilet-cleaning-effect`) %>%
  summarise(mean_rate = mean(rate),
            sd_rate = sd(rate)) %>% 
  ggplot(aes(x = `c-toilet-cleaning-effect`, y = mean_rate)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_rate + sd_rate,
                     lower = mean_rate - sd_rate),
            aes(y = lower), linetype = "dashed")

sens_prop_redist_outbreak_cases <- sens_prop_redist %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>%
  group_by(`random-seed`, `c-toilet-cleaning-effect`) %>% 
  mutate(`outbreak-cases` = `total-hospital-infections` - min(`total-hospital-infections`)) %>% 
  summarise(`outbreak-cases` = max(`outbreak-cases`))

sens_prop_redist_outbreak_cases %>% 
  group_by(`c-toilet-cleaning-effect`) %>% 
  summarise(mean_cases = mean(`outbreak-cases`),
            sd_cases = sd(`outbreak-cases`)) %>% 
  ggplot(aes(x = `c-toilet-cleaning-effect`, y = mean_cases)) +
  geom_line() + 
  # add sd
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = upper), linetype = "dashed") +
  geom_line(data = . %>% 
              mutate(upper = mean_cases + sd_cases,
                     lower = mean_cases - sd_cases),
            aes(y = lower), linetype = "dashed")

list(abx_prescription_rate = sens_abx_prescription_outbreak_cases,
     proportion_redistributed = sens_prop_redist_outbreak_cases) %>%
  bind_rows() %>% 
  pivot_longer(cols = starts_with("c-"),
               names_to = "param",
               values_drop_na = TRUE) %>% 
  group_by(param, value) %>% 
  summarise(mean_cases = mean(`outbreak-cases`),
            sd_cases = sd(`outbreak-cases`)) %>% 
  ggplot(aes(x = value, y = mean_cases)) + 
  geom_line() + 
  facet_wrap(~ param)

