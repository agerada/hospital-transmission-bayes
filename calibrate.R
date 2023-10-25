library(nlrx)
netlogo_path <- "/Applications/NetLogo 6.2.2/"
model_path <- "hospital.nlogo"
out_path <- "out/"

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
                            runtime = 1000,
                            metrics = c("total-colonised"),
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

results <- run_nl_all(nl)
