


n <- 1

res <- system2(guildai:::find_python(),
               c("-c", shQuote(sprintf("print(%i+1)", n))), stdout = TRUE)
stopifnot(identical(res, as.character(n + 1)))

