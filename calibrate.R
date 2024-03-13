library(nlrx)
library(future)
library(tidyverse)
library(lubridate)
netlogo_path <- "/Applications/NetLogo 6.2.2/"
model_path <- "hospital.nlogo"
out_path <- "out/"
sim_days <- 365

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogo_path,
         modelpath = model_path,
         jvmmem = 1024)


nl@experiment <- experiment(expname = "bay_proportions",
                            outpath = out_path,
                            repetition = 1,
                            tickmetrics = "true",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = sim_days,
                            metrics = c("total-colonised",
                                        "total-patients-admitted",
                                        "current-community-infections",
                                        "current-hospital-infections",
                                        "current-inpatients",
                                        "current-colonised"),
                            variables = list("bay-proportion" = list(values = seq(0.1, 0.9, 0.1))),
                            constants = list("wards-total" = 4,
                                             "bedspaces-per-ward" = 12,
                                             "toilet-frequenting-rate" = 2.0,
                                             "toilet-contamination-effect" = 0.2,
                                             "toilet-cleaning-effect" = 0.9,
                                             "toilet-cleaning-rate" = 2,
                                             "community-colonisation-rate" = 0.03,
                                             "discharge-rate" = 0.20))

nl@simdesign <- simdesign_distinct(nl, 1)

plan(list(sequential, multisession))
results <- run_nl_all(nl)

results <- results %>% 
  filter(`[step]` > 0)

dates <- seq(from = date("2024-01-01"), to = date("2024-12-31"), by = 1)
dates <- dates[1:sim_days]
results <- results %>% 
  group_by(siminputrow) %>% 
  mutate(date = dates) %>% 
  mutate(month = month(date))

results %>% 
  group_by(`bay-proportion`, month) %>% 
  summarise(hosp_rate = sum(`current-hospital-infections`) / sum(`current-inpatients`)) %>% 
  ggplot(aes(x = month, y = hosp_rate)) + geom_line() + facet_wrap(~ factor(`bay-proportion`))


# assuming bay proportion 0.6, try to find community rate

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogo_path,
         modelpath = model_path,
         jvmmem = 1024)

nl@experiment <- experiment(expname = "bay_proportions",
                            outpath = out_path,
                            repetition = 1,
                            tickmetrics = "true",
                            idsetup = "setup",
                            idgo = "go",
                            runtime = sim_days,
                            metrics = c("total-colonised",
                                        "total-patients-admitted",
                                        "current-community-infections",
                                        "current-hospital-infections",
                                        "current-inpatients",
                                        "current-colonised"),
                            variables = list("community-colonisation-rate" = list(values = seq(from = 0.01, to = 0.99, length.out = 20)),
                                             ),
                            constants = list("wards-total" = 4,
                                             "bedspaces-per-ward" = 12,
                                             "toilet-frequenting-rate" = 2.0,
                                             "toilet-contamination-effect" = 0.2,
                                             "toilet-cleaning-effect" = 0.9,
                                             "toilet-cleaning-rate" = 2,
                                             "discharge-rate" = 0.20,
                                             "bay-proportion" = 0.6))

nl@simdesign <- simdesign_distinct(nl, 1)
plan(list(sequential, multisession))
results <- run_nl_all(nl)

results <- results %>% 
  filter(`[step]` > 0)

dates <- seq(from = date("2024-01-01"), to = date("2024-12-31"), by = 1)
dates <- dates[1:sim_days]
results <- results %>% 
  group_by(siminputrow) %>% 
  mutate(date = dates) %>% 
  mutate(month = month(date))

results %>% 
  group_by(`community-colonisation-rate`, month) %>% 
  summarise(hosp_rate = sum(`current-hospital-infections`) / sum(`current-inpatients`)) %>% 
  ggplot(aes(x = month, y = hosp_rate)) + geom_line() + facet_wrap(~ factor(`community-colonisation-rate`))

## abc

sim_days <- (365 * 3) + 30

nl <- nl(nlversion = "6.2.2",
         nlpath = netlogo_path,
         modelpath = model_path,
         jvmmem = 1024)

nl@experiment <- experiment(expname = "bay_proportions",
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
                                             #"antibiotic-prescription-rate" = list(min=0.319, max=0.252, qfun='qunif'),
                                             "antibiotic-effect" = list(min=0.01, max=0.99, qfun='qunif'),
                                             "toilet-cleaning-rate" = list(min=1, max=3, qfun='qunif'),
                                             "random-colonisation-thresh" = list(min=0.01, max=0.99, qfun='qunif')),
                            constants = list("wards-total" = 16,
                                             "bedspaces-per-ward" = 14,
                                             #"toilet-cleaning-rate" = 2,
                                             "antibiotic-prescription-rate" = 0.319, # Vesporten 2018
                                             "admission-days" = 8.3,
                                             "bay-proportion" = 0.6))

nl@simdesign <- simdesign_lhs(nl,
                              samples = 1000,
                              nseeds = 1,
                              precision = 3)

plan(list(sequential, multisession))

results <- run_nl_all(nl)
results_bak <- results
results <- results %>% 
  filter(`[step]` > 31)

#dates <- seq(from = ymd("2001-12-01"), to = ymd("2014-12-31"), by = 1)
#dates <- dates[32:sim_days]
results <- results %>% 
  group_by(siminputrow) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_rates <- results %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(siminputrow, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_summary <- results %>% distinct(siminputrow, .keep_all = TRUE) %>% 
  mutate(siminputrow = siminputrow) %>% 
  right_join(results_rates, by=c("siminputrow"))

params_names <- c("community-colonisation-rate",
                  "toilet-contamination-effect",
                  "toilet-cleaning-effect",
                  "toilet-frequenting-rate",
                  #"antibiotic-prescription-rate",
                  "toilet-cleaning-rate",
                  "antibiotic-effect",
                  "random-colonisation-thresh")


salgado <- read_csv("data/salgado_et_al.csv") %>%
  mutate(x = seq(ymd('2002-01-01'), by = '1 month', length.out=nrow(.)))

pre_outbreak_data <- salgado %>% 
  filter(x < ymd("2004-10-01"))

pre_outbreak_sims <- results_summary %>% 
  dplyr::select(all_of(params_names), siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim < ymd("2004-10-01")) %>% 
  dplyr::select(!date_sim) %>% 
  pivot_wider(names_from = c(year, month), values_from = rate,
              names_prefix = "rate_")

pre_target_mean <- mean(pre_outbreak_data$rates)
pre_target_sd <- sd(pre_outbreak_data$rates)

library(abc)
sumstats <- pre_outbreak_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_"))

abc_params <- abc(target = pre_outbreak_data$rates,
    param = pre_outbreak_sims[params_names],
    sumstat = sumstats,
    tol=0.075,
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


ggplot(long_retained_ss) + geom_line(aes(x = x, y = rates, group = i, color = type)) +
  geom_line(data = long_observed_ss, aes(x = x, y = rates, color = type))

ggplot(long_observed_ss, aes(x = x, y = rates)) + geom_line(color = 'red') +
  geom_line(data = sim_mean, aes(x = x, y = m)) +
  geom_line(data = sim_mean, aes(x = x, y = m + sd), linetype='dashed') +
  geom_line(data = sim_mean, aes(x = x, y = m - sd), linetype='dashed')

long_observed_ss %>% 
  mutate(rates = rates - 1) %>% 
ggplot(., aes(x = x, y = rates)) + geom_line() +
  geom_smooth(data = long_observed_ss)

## fine tune

nl@experiment <- experiment(expname = "bay_proportions",
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
                                             "antibiotic-prescription-rate" = list(min=0.25, max=0.252, qfun='qunif'),
                                             "antibiotic-effect" = list(min=0.01, max=0.99, qfun='qunif'),
                                             "random-colonisation-thresh" = list(min=0.01, max=0.99, qfun='qunif')),
                            constants = list("wards-total" = 16,
                                             "bedspaces-per-ward" = 14,
                                             "toilet-cleaning-rate" = 2,
                                             "admission-days" = 8.3,
                                             "bay-proportion" = 0.6))

nl@simdesign <- simdesign_lhs(nl,
                              samples = 200,
                              nseeds = 1,
                              precision = 3)

plan(list(sequential, multisession))

results <- run_nl_all(nl)
results_bak <- results
results <- results %>% 
  filter(`[step]` > 31)

#dates <- seq(from = ymd("2001-12-01"), to = ymd("2014-12-31"), by = 1)
#dates <- dates[32:sim_days]
results <- results %>% 
  group_by(siminputrow) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

results_rates <- results %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(siminputrow, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000)

results_summary <- results %>% distinct(siminputrow, .keep_all = TRUE) %>% 
  mutate(siminputrow = siminputrow) %>% 
  right_join(results_rates, by=c("siminputrow"))

params_names <- c("community-colonisation-rate",
                  "toilet-contamination-effect",
                  "toilet-cleaning-effect",
                  "toilet-frequenting-rate",
                  "antibiotic-prescription-rate",
                  "antibiotic-effect",
                  "random-colonisation-thresh")


salgado <- read_csv("data/salgado_et_al.csv") %>%
  mutate(x = seq(ymd('2002-01-01'), by = '1 month', length.out=nrow(.)))

pre_outbreak_data <- salgado %>% 
  filter(x < ymd("2004-10-01"))

pre_outbreak_sims <- results_summary %>% 
  dplyr::select(all_of(params_names), siminputrow, month, year, rate) %>% 
  mutate(date_sim = ymd(paste(year, month, "01", sep="-"))) %>%
  filter(date_sim < ymd("2004-10-01")) %>% 
  dplyr::select(!date_sim) %>% 
  pivot_wider(names_from = c(month, year), values_from = rate,
              names_prefix = "rate_")

pre_target_mean <- mean(pre_outbreak_data$rates)
pre_target_sd <- sd(pre_outbreak_data$rates)

sumstats <- pre_outbreak_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_"))

abc_params <- abc(target = pre_outbreak_data$rates,
                  param = pre_outbreak_sims[params_names],
                  sumstat = sumstats,
                  tol=0.3,
                  method="rejection")

