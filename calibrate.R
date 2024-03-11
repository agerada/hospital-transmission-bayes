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
                                             "antibiotic-prescription-rate" = list(min=0.25, max=0.252, qfun='qunif'),
                                             "antibiotic-effect" = list(min=0.01, max=0.99, qfun='qunif'),
                                             "random-colonisation-thresh" = list(min=0.01, max=0.99, qfun='qunif')),
                            constants = list("wards-total" = 16,
                                             "bedspaces-per-ward" = 14,
                                             "toilet-cleaning-rate" = 2,
                                             "admission-days" = 8.3,
                                             "bay-proportion" = 0.6))

# nl@simdesign <- simdesign_ABCmcmc_Marjoram(nl, summary_stat_target = c(177,
#                                                                        590,
#                                                                        36,
#                                                                        50,
#                                                                        140,
#                                                                        86),
#                                            n_rec = 1, n_calibration = 1,
#                                            use_seed = TRUE,
#                                            progress_bar = TRUE,
#                                            nseeds = 1)

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

library(abc)
sumstats <- pre_outbreak_sims %>% 
  ungroup %>% 
  dplyr::select(starts_with("rate_"))

abc_params <- abc(target = pre_outbreak_data$rates,
    param = pre_outbreak_sims[params_names],
    sumstat = sumstats,
    tol=0.3,
    method="rejection")

setsim(nl, "simoutput") <- results
input <- getsim(nl, "siminput") %>% 
  dplyr::select(names(getexp(nl, "variables")))
output <- getsim(nl, "simoutput") %>% 
  dplyr::select(getexp(nl, "metrics"))
target <- c("total-colonised" = 177,
            "total-patients-admitted" = 590,
            "curernt-community-infections" = 36,
            "current-hospital-infections" = 50,
            "current-inpatients" = 140,
            "current-colonised" = 86)


results.abc.reject <- abc::abc(target=target, 
                               param=input,
                               sumstat=output,
                               tol=0.3, 
                               method="rejection")

results.abc.loclin <- abc::abc(target=target, 
                               param=input,
                               sumstat=output,
                               tol=0.3, 
                               method="ridge")

results.abc.all <- tibble::as_tibble(results.abc.reject$unadj.values) %>% # results from rejection method
  tidyr::gather(parameter, value) %>% 
  dplyr::mutate(method="rejection") %>% 
  dplyr::bind_rows(tibble::as_tibble(results.abc.loclin$adj.values) %>% # results from local linear regression method
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="loclinear")) %>% 
  dplyr::bind_rows(input %>%                # initial parameter distribution (lhs)
                     tidyr::gather(parameter, value) %>% 
                     dplyr::mutate(method="lhs"))

ggplot2::ggplot(results.abc.all) +
  ggplot2::facet_grid(method~parameter, scales="free") +
  ggplot2::geom_histogram(ggplot2::aes(x=value))

ggplot2::ggplot(results.abc.all) +
  ggplot2::facet_wrap(~parameter, scales="free") +
  ggplot2::geom_density(ggplot2::aes(x=value, fill=method), alpha=0.1)

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

