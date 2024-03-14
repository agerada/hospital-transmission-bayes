
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
