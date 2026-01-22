# Simulating interventions to prevent gastro-intestinal transmission of _Clostridioides difficile_ and antimicrobial-resistant bacteria in healthcare settings: a model calibration study

This repository contains the data, models, and code required to reproduce the
results presented in the above manuscript.

# Requirements

The project `R` requirements are managed using `renv`. To install the required
packages, run the following commands in an `R` console:

```R
install.packages("renv")
renv::restore()
```

`NetLogo` is required to run the simulation model. The model was developed using
version `6.2.2`; therefore, this is the recommended version to use. Download
for your platform from: https://www.netlogo.org/downloads/archive/6.2.2/.

## Mac

This project was originally developed on mac. 
Some packages needed to be built from source, so it is best to first install
`gfortran` and `XQuartz`.

## Conda requirements

Running though conda is not necessary, but may be required on some HPC systems.
Here are some of the required packages (more may be required depending on the
system):

```
r-base
r-essentials
r-xml
r-raster
r-sf 
r-igraph
```

# Running and calibrating the model

Since the model was developed using NetLogo, the easiest way to test it out
is to load NetLogo and open the model file located at
`models/hospital_transmission.nlogo`.

A set of parameters is provided to get started (these are the retained rejection
sampling ABC parameter sets as described in the manuscript). To load these,
from the `Interface` tab, click on `Load Parameter` (bottom left) and select the
file located at `default_params.csv`. [^1]

[^1]: The model uses the mean value of the posterior distributions for each
parameter.

# Poster

The poster uses a `quarto` poster template, which is included. To render:

```bash
quarto render poster/poster.qmd
```

# Paper

The paper currently uses a custom fork of `acronyms` and the `authors-block`
`quarto` extensions, which are included. To render as pdf:

```bash
quarto render paper/paper.qmd --to pdf
```

To render as docx:

```bash
quarto render paper/paper.qmd --to docx
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

