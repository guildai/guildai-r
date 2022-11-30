
y <- 0
for(i in 1:10) {
  y <- y + sample(c(1, -1), 1)
  cat("step:", i, "\n")
  cat("y:", y, "\n")
}
