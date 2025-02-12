
# Replication Material for the Paper “Testing Forecast Rationality for Measures of Central Tendency”

The code in this repository generates the results of the simulations and
the empirical applications of the working paper Dimitriadis, T., Patton,
A.J. and Schmidt, P.W. (2024), Testing Forecast Rationality for Measures
of Central Tendency, available on
[arXiv](https://arxiv.org/abs/1910.12545).

Note that for easy access to functionality and data of paper, you should
use the `fcrat` package. You can find it on [Git-hub
link](https://github.com/Schmidtpk/fcrat). This replication package uses
the `fcrat` package and provides full code for replicating the
simulations and empirical examples of the paper.

## Summary

The `master_script.R` executes all files in the relevant order.

The `packages.R` file helps to prepare the right package versions.

## Requirements

### Software

The code was executed on “R version 4.3.3 (2024-02-29 ucrt)”. To check
if the required R-packages are installed, run the script `packages.R`
and follow the instructions. You can also install the packages at
runtime version with some code provided there using the `groundhog`
package.

### Hardware

The code should run on a standard laptop within 10 Minutes if the
simulations are excluded.

The simulations ran for three days on a server with 128 GB memory and
equipped with an AMD EPYC 7742 64-core processor unit.

## Simulations

The files for the simulations are available in the folder ‘simulations’.
The files starting with ‘sim\_’ carry out the actual simulations and
should be run on a powerful computer/cluster. The files starting with
‘eval\_’ evaluate these simulation results and generate the respective
tables and figures given in the paper.

## Applications

All empirical examples from the main paper are executed in the file
`applications/main application.R`.

The robustness check with clustered covariance can be found in
`applications/sce application with clustered covariance.R`.
