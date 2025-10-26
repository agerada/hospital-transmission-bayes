source("scripts/helpers/simdesign_random_helpers.R")

# Test simdesign_random with a simple example similar to simdesign_lhs

# Setup nl object (using dummy paths for test)
nl <- nlrx::nl(nlversion = "6.2.2",
               nlpath = ".",
               modelpath = "dummy.nlogo")

# Define experiment (adapted from nlrx example)
nl@experiment <- nlrx::experiment(expname = "wolf-sheep-random",
                                  outpath = "/tmp/out/",
                                  repetition = 1,
                                  tickmetrics = "true",
                                  idsetup = "setup",
                                  idgo = "go",
                                  idfinal = NA_character_,
                                  idrunnum = NA_character_,
                                  runtime = 50,
                                  evalticks = seq(40, 50),
                                  metrics = c("count sheep", "count wolves", "count patches with [pcolor = green]"),
                                  variables = list('initial-number-sheep' = list(min = 50, max = 150, qfun = "qunif"),
                                                   'initial-number-wolves' = list(min = 50, max = 150, qfun = "qunif")),
                                  constants = list("model-version" = "\"sheep-wolves-grass\"",
                                                   "grass-regrowth-time" = 30,
                                                   "sheep-gain-from-food" = 4,
                                                   "wolf-gain-from-food" = 20,
                                                   "sheep-reproduce" = 4,
                                                   "wolf-reproduce" = 5,
                                                   "show-energy?" = "false"))

# Attach simdesign_random
nl@simdesign <- simdesign_random(nl = nl,
                                 samples = 10,  # Smaller for test
                                 nseeds = 3,
                                 precision = 3)

# Print summary
print("Simdesign random created successfully")
print(paste("Simmethod:", nl@simdesign@simmethod))
print(paste("Siminput dimensions:", paste(dim(nl@simdesign@siminput), collapse = " x ")))
print(paste("Number of seeds:", length(nl@simdesign@simseeds)))
print("First few rows of siminput:")
print(head(nl@simdesign@siminput))
print("Seeds:")
print(nl@simdesign@simseeds)

# Now, run using lhs
nl@simdesign <- nlrx::simdesign_lhs(nl = nl,
                                    samples = 10,
                                    nseeds = 3,
                                    precision = 3)

# Print summary
print("Simdesign created successfully using LHS")
print(paste("Simmethod:", nl@simdesign@simmethod))
print(paste("Siminput dimensions:", paste(dim(nl@simdesign@siminput), collapse = " x ")))
print(paste("Number of seeds:", length(nl@simdesign@simseeds)))
print("First few rows of siminput:")
print(head(nl@simdesign@siminput))
print("Seeds:")
print(nl@simdesign@simseeds)
