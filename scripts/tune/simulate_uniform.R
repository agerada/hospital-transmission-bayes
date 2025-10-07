sim_days <- (365 * 7) + 30

# outbreak_control_model <- "salgado.nlogo"

nl <- nl(nlversion = nl_version,
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

nl@experiment <- experiment(expname = "uniform_sims",
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
                              samples = calibration_samples,
                              nseeds = calibration_seeds,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  uniform_sims <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(uniform_sims, file.path(out_path, "uniform_sims.rds"))
}

uniform_sims <- read_rds(file.path(out_path, "uniform_sims.rds"))

uniform_sims_bak <- uniform_sims
uniform_sims <- uniform_sims %>% 
  filter(`[step]` > 31)

uniform_sims <- uniform_sims %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

uniform_sims_rates <- uniform_sims %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `siminputrow`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

uniform_sims_rates %>% 
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
pre_outbreak_uniform_sims_rates <- uniform_sims_rates %>% 
  dplyr::filter(date_sim < outbreak_start_date) %>% 
  summarise(across(rate, list(median = median,
                              q_low = \(x) quantile(x, 0.25),
                              q_high = \(x) quantile(x, 0.75),
                              mean = mean)))

outbreak_uniform_sims_rates <- uniform_sims_rates %>% 
  dplyr::filter(date_sim >= outbreak_start_date & date_sim <= outbreak_end_date) %>% 
  summarise(across(rate, list(median = median,
                              q_low = \(x) quantile(x, 0.25),
                              q_high = \(x) quantile(x, 0.75),
                              min = min,
                              max = max,
                              mean = mean)))
