#!/usr/bin/env Rscript


library(tensorflow)

x <- .1

noise <- .3

type <- "foo"

init_phase <- 0+0i


x <- readLines("file.txt")


loss <- (sin(5 * x) * (1 - tanh(x^2)) + rnorm(1) * noise)

cat("loss:", loss, "\n")
