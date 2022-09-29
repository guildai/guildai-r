

# if(getOption("guildai.is_run_active", FALSE)) {
#   cat("In run!\n")
#   stopifnot()
# }

list.files(all.files = TRUE, no.. = TRUE)

x <- .1
noise <- .2

loss <- (sin(5 * x) * (1 - tanh(x^2)) + rnorm(1) * noise)

cat("loss:", loss, "\n")


cat(commandArgs(), "\n")

x = 1
x = 2
print(x)
