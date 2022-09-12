#!/usr/bin/env Rscript
#| requires:
#|   - file: file.txt
#|     description: File dependency
#| output-scalars:
#|   step: 'step: (\value)'
#|   loss: 'loss: (\value)'
#|   accuracy: 'accuracy: (\value)'
#| params:
#|   x2: 3


library(tensorflow)

#| description: "`x` by any other name would smell as sweet."
x <- .1

#| {max: 1, min: 0}
noise <- .3

#| choices: {bar, baz, foo}
type <- "foo"



params <- list(
  xx_key = "xx_val", #| param


  #| param:
  #|   description: "This is the yy_key.
  yy_key = "yy_val"
)

init_phase <- 0+0i


x <- readLines("file.txt")


loss <- (sin(5 * x) * (1 - tanh(x^2)) + rnorm(1) * noise)

cat("loss:", loss, "\n")

library(guildai)
guild_log(loss)
