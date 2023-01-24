# R interface to Guild AI

<!-- badges: start -->

[![R-CMD-check](https://github.com/t-kalinowski/guildai-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/t-kalinowski/guildai-r/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->


<p align="center">
  <img src="vignettes/logos/guild-logo-dark.png" width="50%">
</p>

| <img src="vignettes/logos/TF_FullColor_Stacked.svg" width="100%"/> | <img src="vignettes/logos/tidymodels.png" width="30%"/> |  <img src="vignettes/logos/torch.png" width="33%"/>  |
|:------------------------------------------------------------------:|:-------------------------------------------------------:|:----------------------------------------------------:|
|      <img src="vignettes/logos/keras-logo.png" width="100%"/>      |    <img src="vignettes/logos/mlr3.png" width="32%"/>    | <img src="vignettes/logos/xgboost.png" width="30%"/> |


*guildai* provides a suite of tools for tracking, visualizing, and
managing machine learning experiments. The {guildai} R package is a
successor to the {tfruns} package.

-   Track the hyperparameters, metrics, output, and source code of every
    training run.

-   Compare hyperparmaeters and metrics across runs to find the best
    performing model.

-   No changes to source code required.

# Installation

The R package provides an interface to [Guild AI
Core](https://guild.ai/). The R package will automatically download and
install Guild AI Core on first use, or you can call `install_guild()`
directly to customize the installation.

``` r
# install.packages("guildai")
if(!requireNamespace("remotes"))
  install.packages("remotes", repos = "https://cran.rstudio.com")
remotes::install_github("t-kalinowski/guildai-r")
guildai::install_guild()
```

Guild AI can be used with any machine learning framework, or no
framework at all.

[Get
Started](https://t-kalinowski.github.io/guildai-r/articles/guildai.html)
