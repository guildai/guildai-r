# guildai

<!-- badges: start -->

[![R-CMD-check](https://github.com/t-kalinowski/guildai-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/t-kalinowski/guildai-r/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

*guildai* provides a suite of tools for tracking, visualizing, and
managing training runs and experiments. The {guildai} R package is a
successor to the {tfruns} package designed to work with any machine
learning framework (or even no framework at all).

It lets you:

-   Track the hyperparameters, metrics, output, and source code of every
    training run.

-   Compare hyperparmaeters and metrics across runs to find the best
    performing model.

-   Automatically generate reports to visualize individual training runs
    or comparisons between runs.

-   No changes to source code required.

## Installation

You can install the development version of guildai from Github with:

``` r
# install.packages("remotes")
remotes::install_github("t-kalinowski/guildai-r")
guildai::install_guild()
```
