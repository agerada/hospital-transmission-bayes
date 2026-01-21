# Transmission of hospital acquired infection calibrated using approximate Bayesian computation

NetLogo agent based model of faecal transmission of colonisation within hospital setting: example calibration using Bayesian methods

# Conda requirements

```
r-base
r-essentials
r-xml
r-raster
r-sf 
r-igraph
```

# Poster

To generate poster, install the `quarto` poster template:

```bash
cd poster
quarto use template quarto-ext/typst-templates/poster
```

# Paper

The paper currently uses a fork of `acronyms` that can be installed using:

```bash
quarto add agerada/acronyms@markdown
```

# Model calibration

On a high-level, the calibration process uses the following two step approach:

1. Establish a set of parameters for the pre-outbreak (or baseline) period; this
step can use fairly broad priors. The script for this process is `scripts/tune/pre_outbreak.R`.
2. Use the pre-outbreak (or baseline) posteriors as priors for the calibration
of the outbreak and IPC periods; while it is possible to keep the baseline
parameters fixed, we recommended that they are again calibrated in this step using
`scripts/tune/variable_pre_outbreak.R`.

The following scripts are deprecated and retained to demonstrate alternative
approaches (script may have bugs):

* `scripts/tune/fine_tune_pre_outbreak.R` was used for a second round of 
approximate Bayesian computation (ABC) to further refine the pre-outbreak
parameters
* `scripts/tune/outbreak_and_control.R` kept the pre-outbreak parameters fixed
and focused on calibrating the outbreak and IPC periods

# Calibration tips

