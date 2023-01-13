


n <- 1

python <- Sys.which("python")
if(!nzchar(python))
  python <- Sys.which("python3")

res <- system2(python,
               c("-c", shQuote(sprintf("print(%i+1)", n))), stdout = TRUE)
stopifnot(identical(res, as.character(n + 1)))

