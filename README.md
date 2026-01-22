# Simulating interventions to prevent gastro-intestinal transmission of _Clostridioides difficile_ and antimicrobial-resistant bacteria in healthcare settings: a model calibration study

This repository contains the data, models, and code required to reproduce the
results presented in the above manuscript.

## Quickstart

1. Clone this repository:

```bash
git clone
cd hospital-transmission-bayes
```

2.  Running the interactive model

`NetLogo` is required to run the simulation model. The model was developed using
version `6.2.2`; therefore, this is the recommended version to use. Download
for your platform from: https://www.netlogo.org/downloads/archive/6.2.2/.

The easiest way to test out the model
is to load NetLogo and open the model file located at
`models/hospital_transmission.nlogo`.

A set of parameters is provided to get started (these are the retained rejection
sampling ABC parameter sets as described in the manuscript). To load these,
from the `Interface` tab, click on `Load Parameter` (bottom left) and select the
file located at `models/default_params.csv`. [^1]

[^1]: The model uses the mean value of the posterior distributions for each
parameter.

3. Running the calibration

The model calibration is performed through a collection of `R` scripts located
in the `scripts/` directory. The main entry point for the calibration is the
script `scripts/calibrate.R`, which performs the full calibration as described
in the manuscript.

A quicker calibration run can be performed using the script
`scripts/calibrate_quick.R`. This script is designed to run a small number of
simulations to verify that the calibration process is working and all the
dependencies are correctly installed. It will **not** produce the same results
as in the manuscript (which uses `scripts/calibrate.R`).

The quickest way to run the calibration is through the provided Docker
image.

## Requirements

The project `R` requirements are managed using `renv`. To install the required
packages, run the following commands in an `R` console:

```R
install.packages("renv")
renv::restore()
```

`NetLogo` is required to run the simulation model. The model was developed using
version `6.2.2`; therefore, this is the recommended version to use. Download
for your platform from: https://www.netlogo.org/downloads/archive/6.2.2/.

### Mac

This project was originally developed on mac. 
Some packages needed to be built from source, so it is best to first install
`gfortran` and `XQuartz`.

### Conda requirements

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

## Docker Setup

A Docker configuration is provided to run the calibration in a containerized environment with all dependencies pre-installed.

### Prerequisites

- Docker installed on your system
- At least 4GB of available RAM (recommended)

### Quick Start

1. **Run quick calibration (script is provided as an argument):**
   ```bash
   ./run_calibration.sh scripts/calibrate_quick.R
   ```

2. **Run full calibration:**
   ```bash
   ./run_calibration.sh scripts/calibrate.R
   ```

### Manual Docker Commands

1. **Build the Docker image:**
   ```bash
   docker-compose build
   ```

2. **Run any calibration script by passing the script path as the container argument:**
   ```bash
   # quick
   docker-compose run --rm calibration scripts/calibrate_quick.R

   # full
   docker-compose run --rm calibration scripts/calibrate.R
   ```

Notes:
- The Docker image contains only system and R package dependencies (NetLogo is installed in the image).
- Project files (including `scripts/`) are mounted into the container at runtime, so edits to `scripts/*.R` in your repository are reflected immediately when you run the container (no need to rebuild the image for code changes).
- The container disables `renv` autoloader to avoid attempts to write into the mounted project; if you need to change package versions, update `renv.lock` and rebuild the image with `docker-compose build`.
- A compatibility symlink for Java is provided inside the image to support NetLogo headless scripts that expect `/usr/lib/jvm/java-1.17.0-openjdk-amd64`.

## Output

Calibration results will be saved to the mounted output directory on your host (`out/` in the project root).
The outputs are the following:
- abc_params.csv: retained parameter sets from ABC calibration
- calibrate.RData: R workspace image with calibration results (used in `paper/paper.qmd` for analysis and figures)
- individual experiments `.rds` files.

## Render

### Poster

The poster uses a `quarto` poster template, which is included. To render:

```bash
quarto render poster/poster.qmd
```

### Paper

The paper currently uses a custom fork of `acronyms` and the `authors-block`
`quarto` extensions, which are included. To render as pdf:

```bash
quarto render paper/paper.qmd --to pdf
```

To render as docx:

```bash
quarto render paper/paper.qmd --to docx
```

## Model calibration

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

## Citation

To follow.

## Contact

[Dr Alessandro Gerada](mailto:alessandro.gerada@liverpool.ac.uk)

## Licence

This repository and its contents are licensed under the **Creative Commons
Attribution 4.0 International (CC BY 4.0)** license. See the `LICENSE` file for more
details.
