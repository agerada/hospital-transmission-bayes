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
                            constants = estimated_consts_fixed)

nl@simdesign <- simdesign_simple(nl,
                              nseeds = calibration_seeds)

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
