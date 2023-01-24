# R interface to Guild AI

<!-- badges: start -->

<div>

[![](https://github.com/t-kalinowski/guildai-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/t-kalinowski/guildai-r/actions/workflows/R-CMD-check.yaml)

R-CMD-check

</div>

<!-- badges: end   -->

|                                                           |
|:---------------------------------------------------------:|
| ![](vignettes-src/logos/guild-logo-dark.png){width="50%"} |

|                                                                        |                                                             |                                                          |
|:----------------------------------------------------------------------:|:-----------------------------------------------------------:|:--------------------------------------------------------:|
| ![](vignettes-src/logos/TF_FullColor_Stacked.svg){style="width:100%;"} | ![](vignettes-src/logos/tidymodels.png){style="width:65%;"} |  ![](vignettes-src/logos/torch.png){style="width:70%;"}  |
|      ![](vignettes-src/logos/keras-logo.png){style="width:80%;"}       |    ![](vignettes-src/logos/mlr3.png){style="width:70%;"}    | ![](vignettes-src/logos/xgboost.png){style="width:90%;"} |

<!-- a nice screenshot image here-->

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
