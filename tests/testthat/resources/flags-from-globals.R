#| echo: false

i <- 123L
f <- 1.123
s <- "Howdy Guild"
  b <-
  # a comment in the middle
  FALSE # a trailing comment
# a comment after
#
#
    s2 <- r"(foo)"
      s3 <- r"(
    foobarbaz
    )"
cx <- 1 +
      1i

print(guildai:::as_yaml(environment()))
