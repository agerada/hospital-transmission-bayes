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
                            variables = list(
                              "c-antibiotic-prescription-rate" = list(
                                values = seq(from = 0.01, to = 0.99, length.out = sens_samples)
                              )
                            )
)

nl@simdesign <- simdesign_distinct(nl,
                                   nseeds = sens_seeds)

plan(list(sequential, future_plan), workers = num_workers)

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
