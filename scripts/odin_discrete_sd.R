# Discrete stochastic S/I hospital model in odin

# -----------------------
# Initial conditions
initial(S) <- S0
initial(I) <- I0

S0 <- user()
I0 <- user()

# -----------------------
# Parameters
A  <- user()   # admission rate (total per dt)
D  <- user()   # discharge rate (total per dt)
r  <- user()   # spontaneous colonisation rate
dt <- user()   # time step (e.g. 1 day)

# -----------------------
# Forcings (time-varying inputs)
dim(beta) <- user()
dim(p)    <- user()

beta[] <- user()
p[]    <- user()

# At each discrete step, interpolate forcings
beta_t <- interpolate(beta, step)
p_t    <- interpolate(p, step)

# -----------------------
# State updates
update(S) <- S + adm_S - n_discharge_S - n_SI - n_spont
update(I) <- I + adm_I - n_discharge_I + n_SI + n_spont

# -----------------------
# Admissions (Poisson)
adm_S <- rpois(p_t * A * dt)
adm_I <- rpois((1 - p_t) * A * dt)

# -----------------------
# Discharges and transitions
total <- S + I
total_nonzero <- if (total > 0) total else 1.0

# --- Susceptible competing hazards
lambda_dis_S <- D / total_nonzero
lambda_inf_S <- beta_t
lambda_r_S   <- r
Lambda_S     <- lambda_dis_S + lambda_inf_S + lambda_r_S

# Probability that a susceptible leaves this step
p_leave_S <- 1 - exp(-Lambda_S * dt)

# Number leaving susceptibles
n_leave_S <- rbinom(S, p_leave_S)

# Allocate leaving susceptibles among outcomes
dim(probs_S) <- 3
dim(alloc_S) <- 3

probs_S[1] <- if (Lambda_S > 0) lambda_dis_S / Lambda_S else 0.0
probs_S[2] <- if (Lambda_S > 0) lambda_inf_S / Lambda_S else 0.0
probs_S[3] <- if (Lambda_S > 0) lambda_r_S   / Lambda_S else 0.0

alloc_S[] <- rmultinom(n_leave_S, probs_S)

n_discharge_S <- alloc_S[1]
n_SI          <- alloc_S[2]
n_spont       <- alloc_S[3]

# --- Infected departures (only discharge in this model)
lambda_dis_I <- D / total_nonzero
p_leave_I    <- 1 - exp(-lambda_dis_I * dt)
n_leave_I    <- rbinom(I, p_leave_I)
n_discharge_I <- n_leave_I
