

x <- .1
noise <- .2

loss <- (sin(5 * x) * (1 - tanh(x   ^    2)) + rnorm(1) * noise)

# An innocuous comment
#





cat("loss:", loss, "\n")

