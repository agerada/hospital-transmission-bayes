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
