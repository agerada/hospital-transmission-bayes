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
                              samples = sens_samples,
                              nseeds = sens_seeds,
                              precision = 3)

plan(list(sequential, multisession))

if (run_sims) {
  sens_toilet_cleaning_effect <- progressr::with_progress(
    run_nl_all(nl))
  write_rds(sens_toilet_cleaning_effect, file.path(out_path, "sens_toilet_cleaning_effect.rds"))
}

sens_toilet_cleaning_effect <- read_rds(file.path(out_path, "sens_toilet_cleaning_effect.rds"))

sens_toilet_cleaning_effect_bak <- sens_toilet_cleaning_effect

sens_toilet_cleaning_effect <- sens_toilet_cleaning_effect %>% 
  filter(`[step]` > 31)

sens_toilet_cleaning_effect <- sens_toilet_cleaning_effect %>% 
  group_by(`random-seed`, `siminputrow`) %>% 
  mutate(date_sim = seq(from = ymd("2002-01-01"),
                        by = "1 day",
                        length.out = n()))

sens_toilet_cleaning_effect_rates <- sens_toilet_cleaning_effect %>% 
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

sens_toilet_cleaning_effect_outbreak_rates <- sens_toilet_cleaning_effect %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>% 
  group_by(`random-seed`, `c-toilet-cleaning-effect`, year, month) %>% 
  summarise(rate = (max(`total-hospital-infections`) - min(`total-hospital-infections`)) / sum(`current-inpatients`) * 1000) %>% 
  mutate(date_sim = ymd(paste0(year, "-", month, "-01"))) %>% 
  ungroup() %>% 
  dplyr::select(!c(year, month))

sens_toilet_cleaning_effect_outbreak_rates %>% 
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

sens_toilet_cleaning_effect_outbreak_cases <- sens_toilet_cleaning_effect %>% 
  filter(`[step]` >= filter_from & `[step]` <= filter_to) %>% 
  mutate(year = year(date_sim), month = month(date_sim)) %>%
  group_by(`random-seed`, `c-toilet-cleaning-effect`) %>% 
  mutate(`outbreak-cases` = `total-hospital-infections` - min(`total-hospital-infections`)) %>% 
  summarise(`outbreak-cases` = max(`outbreak-cases`))

sens_toilet_cleaning_effect_outbreak_cases %>% 
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
