library(nlrx)
library(future)
library(tidyverse)
library(lubridate)
library(abc)
library(progressr)
library(here)
handlers("cli")
options(progressr.enable=TRUE)
source(here::here("scripts", "helpers", "simdesign_random_helpers.R"))

# # optionally, set jvm here:
# Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-1.17.0-openjdk-amd64/")

# set parameter sampling functions
simdesign_fn <- simdesign_random

# set future plan (multicore or multisession) and workers
future_plan <- "multicore"
num_workers <- availableCores() - 2

# set ward design - SMALL HOSPITAL FOR QUICK TESTING
wards_total <- 4
bedspaces_per_ward <- 4

# set random seed for reproducibility
set.seed(42)

netlogo_path <- Sys.getenv("NETLOGO_HOME", here::here("netlogo"))
nl_version <- "6.2.2"

model_path <- here::here("models", "hospital_transmission.nlogo")
out_path <- here::here("out")
data_path <- here::here("data")

# previous versions of this script used this option to skip running the
# simulations themselves, and load previously saved results instead into
# R objects. Now, the script saves everything to a single .RData file at the end,
# so this option is less useful, but is retained for backwards compatibility.
run_sims <- TRUE

antibiotic_prescription_rate = 0.499 # Vesporten 2018
admission_days <- 4.6
bay_proportion <- 0.13

if (!dir.exists(out_path)) {
  dir.create(out_path)
}

## Finally, adjust the number of samples and seeds for each calibration below.
## Either set to a fixed number, or use availableCores() * n to scale with the
## number of available CPU cores.

##============== pre-outbreak =============##

calibration_samples <- 50  # REDUCED FOR QUICK TESTING (need enough for ABC to accept some)
# calibration_samples <- availableCores() * 8
calibration_seeds <- 1  # REDUCED FOR QUICK TESTING
abc_tol <- 0.2  # INCREASED FOR QUICK TESTING (to accept ~10 samples)

message("Running pre-outbreak calibration")
message(glue::glue("Calibration samples: {calibration_samples},
                   Calibration seeds: {calibration_seeds},
                   ABC tolerance: {abc_tol}"))
source(here::here("scripts", "tune", "pre_outbreak.R"))

##============== outbreak and control ===============##

# calibration_samples <- 1000
# calibration_seeds <- 3
# abc_tol <- 0.025
# message("Running outbreak and control calibration")
# message(glue::glue("Calibration samples: {calibration_samples},
#                    Calibration seeds: {calibration_seeds},
#                    ABC tolerance: {abc_tol}"))
# source(here::here("scripts", "tune", "outbreak_and_control.R"))

##=========== fine tune pre-outbreak ===================================##

# calibration_samples <- 1000
# calibration_seeds <- 3
# abc_tol <- 0.025
# message("Running fine tune pre-outbreak calibration")
# message(glue::glue("Calibration samples: {calibration_samples},
#                    Calibration seeds: {calibration_seeds},
#                    ABC tolerance: {abc_tol}"))
# source(here::here("scripts", "tune", "fine_tune_pre_outbreak.R"))

##=========== use variables for pre-outbreak params as well ============##

calibration_samples <- 50  # REDUCED FOR QUICK TESTING (need enough for ABC to accept some)
# calibration_samples <- availableCores() * 10
calibration_seeds <- 1  # REDUCED FOR QUICK TESTING
abc_tol <- 0.2  # INCREASED FOR QUICK TESTING (to accept ~10 samples)

upper_bound <- "mean" # mean, min, or max (of pre-outbreak priors)
upper_bound_scale <- 1 # scale the upper bound by this factor

lower_bound <- "mean" # mean, min, or max (of pre-outbreak priors)
lower_bound_scale <- 1 # scale the lower bound by this factor

message("Running pre-outbreak calibration with variables")
message(glue::glue("Calibration samples: {calibration_samples},
                   Calibration seeds: {calibration_seeds},
                   ABC tolerance: {abc_tol}"))
source(here::here("scripts", "tune", "variable_pre_outbreak.R"))

##=============== simulate by uniform sampling from posterior ===============##
# This draws parameters from the posterior means +/- SD
# These parameter sets are used as inputs to outbreak model

calibration_samples <- 5  # REDUCED FOR QUICK TESTING
# calibration_samples <- availableCores() * 1
calibration_seeds <- 2  # REDUCED FOR QUICK TESTING
source(here::here("scripts", "tune", "simulate_uniform.R"))

## If you want to change the hospital size for the posterior predictive checks and sensitivity
## analyses, adjust the parameters here:
# wards_total <- 24
# bedspaces_per_ward <- 18

###========== single outbreak only simulation =========###
# This uses the posterior means as inputs to a single outbreak

calibration_seeds <- 5  # REDUCED FOR QUICK TESTING
# calibration_seeds <-availableCores() * 1
source(here::here("scripts", "tune", "simulate_mean.R"))

# save.image(file.path(out_path, "calibrate.RData"))

##================= posterior predictive checks ==================##

calibration_samples <- 5  # REDUCED FOR QUICK TESTING
# calibration_samples <- availableCores() * 1
source(here::here("scripts", "tune", "posterior_predictive_checks.R"))

##================= sensitivity analysis ==================##

sens_samples <- 5  # REDUCED FOR QUICK TESTING
# sens_samples <- availableCores() * 1
sens_seeds <- 1  # REDUCED FOR QUICK TESTING
source(here::here("scripts", "sensitivity", "abx_prescription_rates.R"))

#####============= sens proportion redistributed

source(here::here("scripts", "sensitivity", "proportion_redistributed.R"))

#####============= sens toilet cleaning rate ==================##

source(here::here("scripts", "sensitivity", "toilet_cleaning_rate.R"))

####================ sensitivity toilet cleaning effect ==============####

source(here::here("scripts", "sensitivity", "toilet_cleaning_effect.R"))

##================= sensitivity analysis with perfect clean ==============##

# source(here::here("scripts", "sensitivity", "proportion_redistributed_perfect_clean.R"))

# list(abx_prescription_rate = sens_abx_prescription_outbreak_cases,
#      proportion_redistributed = sens_prop_redist_outbreak_cases,
#      toilet_cleaning_rate = sens_toilet_cleaning_rate_outbreak_cases,
#      toilet_cleaning_effect = sens_toilet_cleaning_effect_outbreak_cases) %>%
#   bind_rows() %>% 
#   pivot_longer(cols = starts_with("c-"),
#                names_to = "param",
#                values_drop_na = TRUE) %>% 
#   group_by(param, value) %>% 
#   summarise(mean_cases = mean(`outbreak-cases`),
#             sd_cases = sd(`outbreak-cases`)) %>% 
#   ggplot(aes(x = value, y = mean_cases)) + 
#   geom_line() + 
#   facet_wrap(~ param, scales = "free")

####================= sensitivity analysis abx prescription rates preoutbreak ==============####

source(here::here("scripts", "sensitivity", "abx_prescription_rates_pre_outbreak.R"))

### save all objects
save.image(file.path(out_path, "calibrate.RData"))