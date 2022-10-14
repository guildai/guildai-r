#| echo: true

i <- 123L
f <- 1.123
s <- "Howdy Guild"

# make sure guild can handle expressions that span more than one line, or
# have loitering comments, or odd whitespace indentation, or complex
# multiline string literals
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
#

# make sure guild can handle multiple globals defined on one line
# here, b2, s4, and s5 are flags, not_a_global and b2 are not
b2 <- TRUE; s4 = "A string"; not_a_global <- b2; b2; s5 <- "Another string";


# guild should always modify just the first global flag assignment location
duplicated_flag = 1L
duplicated_flag = -1L
stopifnot(identical(duplicated_flag, -1L))

# complex flags are a little tricky, extra tests:
cx2 <- 2+2i
cx3 <- 3 + 3i

globals <- mget(ls())

i2 <- - 123L # test token substitution on literals that
i3 <- + 123L # are really unary calls of `-` or `+`

{
  cat("R exprs:\n")
  for (nm in names(globals))
    cat("  ", nm, " = ", deparse1(get(nm)), "\n", sep = "")
}

# echo can be turned off mid-run
# supports the same behavior as `R -f file.R` at the command line
options(echo = FALSE)

guildai:::print.yaml(guildai:::as_yaml(list(YAML = globals)))
