#| echo: false

cat("position:", position <- 0, "\n")
for(i in 1:10) {
  position <- position + sample(c(1, -1), 1)
  cat("step:", i, "\n")
  cat("position:", position, "\n")
}
