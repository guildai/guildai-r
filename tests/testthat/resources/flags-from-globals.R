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
cx <- 0 +
      0i

    # comment
cx1 <- 1 + # comment
      1i   # comment
# comment


globals <- mget(ls())

dput(globals)

guildai:::print.yaml(guildai:::as_yaml(globals))
