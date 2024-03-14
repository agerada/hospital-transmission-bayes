library(nlrx)
library(future)
library(tidyverse)
library(lubridate)
library(abc)
netlogo_path <- "/Applications/NetLogo 6.2.2/"
model_path <- "hospital.nlogo"
out_path <- "out/"
wards_total <- 16
bedspaces_per_ward <- 14

##============== pre-outbreak =============##

sim_days <- (365 * 3) + 30

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
                                             "toilet-frequenting-rate" = list(min=0.5, max=12, qfun='qunif'),
                                             "antibiotic-effect" = list(min=0.01, max=0.99, qfun='qunif'),
                                             "toilet-cleaning-rate" = list(min=1, max=3, qfun='qunif'),
                                             "random-colonisation-thresh" = list(min=0.01, max=0.99, qfun='qunif')),
                            constants = list("wards-total" = wards_total,
                                             "bedspaces-per-ward" = bedspaces_per_ward,
                                             "antibiotic-prescription-rate" = 0.319, # Vesporten 2018
                                             "admission-days" = 8.3,
                                             "bay-proportion" = 0.6))

nl@simdesign <- simdesign_lhs(nl,
                              samples = 1000,
                              nseeds = 1,
                              precision = 3)

plan(list(sequential, multisession))

results_baseline <- progressr::with_progress(
  run_nl_all(nl))
results_baseline_bak <- results_baseline
results_baseline <- results_baseline %>% 
  filter(`[step]` > 31)

results_baseline <- results_baseline %>% 
  group_by(siminputrow) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_baseline_rates <- results_baseline %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(siminputrow, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_baseline_summary <- results_baseline %>% distinct(siminputrow, .keep_all = TRUE) %>% 
  mutate(siminputrow = siminputrow) %>% 
  right_join(results_baseline_rates, by=c("siminputrow"))

params_names_pre_outbreak <- names(nl@experiment@variables)

salgado <- read_csv("data/salgado_et_al.csv") %>%
  mutate(x = seq(ymd('2002-01-01'), by = '1 month', length.out=nrow(.)))

pre_outbreak_data <- salgado %>% 
  filter(x < ymd("2004-10-01"))

pre_outbreak_sims <- results_baseline_summary %>% 
  dplyr::select(all_of(params_names_pre_outbreak), siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim < ymd("2004-10-01")) %>% 
  dplyr::select(!date_sim) %>% 
  pivot_wider(names_from = c(year, month), values_from = rate,
              names_prefix = "rate_")

sumstats <- pre_outbreak_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_"))

abc_params <- abc(target = pre_outbreak_data$rates,
                  param = pre_outbreak_sims[params_names_pre_outbreak],
                  sumstat = sumstats,
                  tol=0.025,
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

outbreak_control_model <- "salgado.nlogo"

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogo_path,
         modelpath = outbreak_control_model,
         jvmmem = 1024)

consts <- list("wards-total" = wards_total,
               "bedspaces-per-ward" = bedspaces_per_ward,
               "antibiotic-prescription-rate" = 0.319, # Vesporten 2018
               "admission-days" = 8.3,
               "bay-proportion" = 0.6)

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
                                                              max=12,
                                                              qfun='qunif'),
                           "o-toilet-contamination-effect" = list(min=consts[["toilet-contamination-effect"]],
                                                                  max=0.99,
                                                                  qfun='qunif'),
                           "o-toilet-cleaning-effect" = list(min=0.01,
                                                             max=consts[["toilet-cleaning-effect"]],
                                                             qfun='qunif'),
                           "o-toilet-cleaning-rate" = list(min=1,
                                                           max=consts[["toilet-cleaning-rate"]],
                                                           qfun='qunif'),
                           "o-community-colonisation-rate" = list(min=consts[["community-colonisation-rate"]],
                                                                  max=0.1,
                                                                  qfun='qunif'),
                           "o-antibiotic-prescription-rate" = list(min=0.319,
                                                                   max=0.62, # also from Vesporten 2018
                                                                   qfun='qunif'),
                           "c-toilet-cleaning-effect" = list(min=consts[["toilet-cleaning-effect"]],
                                                             max=0.99,
                                                             qfun='qunif'),
                           "c-toilet-cleaning-rate" = list(min=consts[["toilet-cleaning-rate"]],
                                                           max=3,
                                                           qfun='qunif'),
                           "c-antibiotic-prescription-rate" = list(min=0.237,
                                                                   max=0.319,
                                                                   qfun='qunif'))


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
                              samples = 1000,
                              nseeds = 1,
                              precision = 3)

plan(list(sequential, multisession))

results_outbreak_control <- progressr::with_progress(
  run_nl_all(nl))
results_outbreak_control_bak <- results_outbreak_control
results_outbreak_control <- results_outbreak_control %>% 
  filter(`[step]` > 31)

results_outbreak_control <- results_outbreak_control %>% 
  group_by(siminputrow) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_outbreak_control_rates <- results_outbreak_control %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(siminputrow, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_outbreak_control_summary <- results_outbreak_control %>% distinct(siminputrow, .keep_all = TRUE) %>% 
  mutate(siminputrow = siminputrow) %>% 
  right_join(results_outbreak_control_rates, by=c("siminputrow"))

params_names_outbreak_control <- names(nl@experiment@variables)

# salgado <- read_csv("data/salgado_et_al.csv") %>%
#   mutate(x = seq(ymd('2002-01-01'), by = '1 month', length.out=nrow(.)))

# pre_outbreak_data <- salgado %>% 
#   filter(x < ymd("2004-10-01"))

outbreak_control_sims <- results_outbreak_control_summary %>% 
  dplyr::select(all_of(params_names_outbreak_control), siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim <= max(salgado$x)) %>% 
  dplyr::select(!date_sim) %>% 
  pivot_wider(names_from = c(year, month), values_from = rate,
              names_prefix = "rate_")

sumstats <- outbreak_control_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_"))

abc_params_outbreak_control <-abc(target = salgado$rates,
                                  param = outbreak_control_sims[params_names_outbreak_control],
                                  sumstat = sumstats,
                                  tol=0.025,
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



##=========== use variables for pre-outbreak params as well ============##

sim_days <- (365 * 7) + 30

outbreak_control_model <- "salgado.nlogo"

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogo_path,
         modelpath = outbreak_control_model,
         jvmmem = 1024)

consts <- list("wards-total" = wards_total,
               "bedspaces-per-ward" = bedspaces_per_ward,
               "antibiotic-prescription-rate" = 0.319, # Vesporten 2018
               "admission-days" = 8.3,
               "bay-proportion" = 0.6)

# set outbreak and control
consts <- c(consts, "outbreak?" = TRUE, "infection-control?" = TRUE)

# Set control measures start, which in Salgado was third week of November.
# Calculate how many ticks into simulation this would be
eicm_start <- as.numeric(difftime(ymd('2004-11-15'), ymd('2002-01-01'), units = 'days'))
consts <- c(consts, "control-start" = eicm_start)

# in Salgado, the outbreak starts in October 2004, calculate how many ticks/days
# into simulation this is, then offset by 90 days in the search field
outbreak_start <- as.numeric(difftime(ymd('2004-10-01'), ymd('2002-01-01'), units = 'days'))

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
                           "o-toilet-frequenting-rate" = list(min=pre_outbreak_variables[["toilet-frequenting-rate"]]$min,
                                                              max=8,
                                                              qfun='qunif'),
                           "o-toilet-contamination-effect" = list(min=pre_outbreak_variables[["toilet-contamination-effect"]]$min,
                                                                  max=0.99,
                                                                  qfun='qunif'),
                           "o-toilet-cleaning-effect" = list(min=0.01,
                                                             max=pre_outbreak_variables[["toilet-cleaning-effect"]]$max,
                                                             qfun='qunif'),
                           "o-toilet-cleaning-rate" = list(min=1,
                                                           max=pre_outbreak_variables[["toilet-cleaning-rate"]]$max,
                                                           qfun='qunif'),
                           "o-community-colonisation-rate" = list(min=pre_outbreak_variables[["community-colonisation-rate"]]$min,
                                                                  max=0.1,
                                                                  qfun='qunif'),
                           "o-antibiotic-prescription-rate" = list(min=0.319,
                                                                   max=0.62, # also from Vesporten 2018
                                                                   qfun='qunif'),
                           "c-toilet-cleaning-effect" = list(min=pre_outbreak_variables[["toilet-cleaning-effect"]]$min,
                                                             max=0.99,
                                                             qfun='qunif'),
                           "c-toilet-cleaning-rate" = list(min=pre_outbreak_variables[["toilet-cleaning-rate"]]$min,
                                                           max=3,
                                                           qfun='qunif'),
                           "c-antibiotic-prescription-rate" = list(min=0.237,
                                                                   max=0.319,
                                                                   qfun='qunif'))


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
                              samples = 1000,
                              nseeds = 1,
                              precision = 3)

plan(list(sequential, multisession))

results_outbreak_control <- progressr::with_progress(
  run_nl_all(nl))
results_outbreak_control_bak <- results_outbreak_control
results_outbreak_control <- results_outbreak_control %>% 
  filter(`[step]` > 31)

results_outbreak_control <- results_outbreak_control %>% 
  group_by(siminputrow) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_outbreak_control_rates <- results_outbreak_control %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(siminputrow, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_outbreak_control_summary <- results_outbreak_control %>% distinct(siminputrow, .keep_all = TRUE) %>% 
  mutate(siminputrow = siminputrow) %>% 
  right_join(results_outbreak_control_rates, by=c("siminputrow"))

params_names_outbreak_control <- names(nl@experiment@variables)

outbreak_control_sims <- results_outbreak_control_summary %>% 
  dplyr::select(all_of(params_names_outbreak_control), siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim <= max(salgado$x)) %>% 
  dplyr::select(!date_sim) %>% 
  pivot_wider(names_from = c(year, month), values_from = rate,
              names_prefix = "rate_")

sumstats <- outbreak_control_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_"))

abc_params_outbreak_control <-abc(target = salgado$rates,
                                  param = outbreak_control_sims[params_names_outbreak_control],
                                  sumstat = sumstats,
                                  tol=0.025,
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

