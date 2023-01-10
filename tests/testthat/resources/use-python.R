
# Sys.which("python3")

n <- 1

res <- system2("python3", c("-c", shQuote(sprintf("print(%i+1)", n))), stdout = TRUE)
stopifnot(identical(res, as.character(n + 1)))

