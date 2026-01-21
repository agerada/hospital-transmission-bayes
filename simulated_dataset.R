library(tidyverse)

starting_population = 1e4
baseline_population_resistance_rate <- 0.1
n_admissions_lambda <- 1
prescription_rate <- 0.1
alpha <- 0.1
beta <- 0.1
random_error_sigma <- 0.1

number_of_admissions <- rpois(starting_population, n_admissions_lambda)
patient_nos <- sample.int(n = 1e4, size = starting_population)
patient_nos <- sprintf("%06d", patient_nos)
sex <- rbinom(starting_population, size = 2,
                  prob=c(0.45, 0.45, 0.1)) + 1
sex <- c("male", "female", "U")[sex]
age <- sample(18:99, starting_population, replace = TRUE)

baseline_resistance <- rbinom(n = starting_population,
                              size = 1,
                              prob = baseline_population_resistance_rate)
patient_df <- data.frame(patient_nos,
                         sex,
                         age)

# 
# baseline_resistance <- as.logical(baseline_resistance)
# baseline_df <- data.frame(patient_id = patient_nos,
#                           community_acquired_infection = baseline_resistance)
# 
admissions <- rep(patient_nos, times = number_of_admissions)
sex <- rep(sex, times = number_of_admissions)
age <- rep(age, times = number_of_admissions)
admission_df <- data.frame(patient_id = admissions,
                           age = age,
                           sex = sex)

# admission_df <- merge(admission_df, baseline_df)
# 
# admission_df['on_antibiotics'] <- rpois(nrow(admission_df),
#                                         1)
# 
# hospital_df <- admission_df %>% 
#   group_by(patient_id) %>% 
#   mutate(colonisation_risk = seq(length(patient_id)) * alpha + on_antibiotics * beta) %>% 
#   mutate(hospital_acquired_infection = ( colonisation_risk + rnorm(1, 0 , random_error_sigma) ) > 0.5) %>% 
#   mutate(colonisation = community_acquired_infection | hospital_acquired_infection)
# 
# hospital_df %>%
#   group_by(patient_id) %>%
#   mutate(n_adm = length(patient_id)) %>%
#   lm(colonisation ~ n_adm, data = .)

admission_df <- admission_df %>% 
  group_by(patient_id) %>% 
  mutate(date_admission = seq(length(patient_id)))
baseline_cip_res <- 0.1
admission_df['ciprofloxacin'] <- rbinom(n = nrow(admission_df), size = 1, prob = baseline_cip_res)

gent_theta <- 0.5
gent_age_theta <- 0.8
gent_error_sig <- 0.05
gent_acquired <- admission_df[['ciprofloxacin']] * gent_theta + (admission_df[['age']] / 100) * gent_age_theta + rnorm(nrow(admission_df), 0, gent_error_sig)
baseline_gent_res <- 0.1
baseline_gent <- rbinom(n = nrow(admission_df), size = 1, prob = baseline_gent_res)
admission_df['gentamicin'] <- pmax(gent_acquired, baseline_gent)
admission_df['gentamicin'] <- if_else(admission_df['gentamicin'] > 0.5, "R", "S")

amc_theta <- 0.5
amc_error_sig <- 0.05
amc_acquired <- admission_df[['ciprofloxacin']] * amc_theta + rnorm(nrow(admission_df), 0, amc_error_sig)
baseline_amc_res <- 0.1
baseline_amc <- rbinom(n = nrow(admission_df), size = 1, prob = baseline_amc_res)
admission_df['amoxicillin_clavulanic_acid'] <- pmax(amc_acquired, baseline_amc)
admission_df['amoxicillin_clavulanic_acid'] <- if_else(admission_df['amoxicillin_clavulanic_acid'] > 0.5, "R", "S")

admission_df['ciprofloxacin'] <- if_else(admission_df['ciprofloxacin'] == 1,
                                         "R", "S")

admission_df %>% group_by(amoxicillin_clavulanic_acid, ciprofloxacin, gentamicin) %>% summarise(n = n())

admission_df %>% group_by(amoxicillin_clavulanic_acid) %>% summarise(per = 100 * n() / nrow(.))

admission_df %>% filter(age > 50) %>% group_by(gentamicin) %>% summarise(n = n() / nrow(.))

boxplot(admission_df$age ~ as.factor(admission_df$gentamicin))

admission_df <- admission_df[sample(nrow(admission_df)), ]
admission_df %>% 
  select(-age, -sex) %>% 
  write_csv("admissions.csv")

patient_df %>% 
  write_csv("patients.csv")
