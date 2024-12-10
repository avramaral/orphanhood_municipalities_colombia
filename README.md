# Orphanhood modelling

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

## About this repository

This repository contains the code the project **Modelling orphanhood levels in Colombia, based on mortality and fertility data**.

### Contents

-   `DATA`: contains all data necessary to compute orphanhood.
-   `docs`: contains a report with the summary of the results.
-   `FITTED`: contains the fitted models.
-   `HPC_script`: contains `.sh` files to be run on the Imperial HPC.
-   `IMAGES`: contains generated plots for fertility and mortality analyses.
-   `ORPHANHOOD`: contains all orphanhood-related generated files.
-   `R`: contains all `.R` scripts.
-   `STAN`: contains all `.stan` scripts.
-   `TEST`: contains a minimal example to test access to the Imperial HPC.

## Installation

Clone the repository to your chosen directory on your local machine.

``` bash
git clone git@github.com:avramaral/orphanhood_municipalities_colombia.git
```

## Quick Start

### Setup

If running only the `.stan` models and orphanhood scripts, execute `install-dependencies-hpc.R`.

``` bash
Rscript install-dependencies-hpc.R
```

To install all dependencies (including libraries for plotting and post-processing), execute `install-dependencies-hpc.R`.

``` bash
Rscript install-dependencies.R
```
