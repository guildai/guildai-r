# guild run w/ flags-dest: globals

    > #| echo: true
    > 
    > i <- 123L
    > f <- 1.123
    > s <- "Howdy Guild"
    > # make sure guild can handle expressions that span more than one line, or
    > # have loitering comments, or odd whitespace indentation, or complex
    > # multiline string literals
    >   b <-
    +   # a comment in the middle
    +   FALSE # a trailing comment
    > # a comment after
    > #
    > #
    >     s2 <- r"(foo)"
    >       s3 <- r"(
    +     foobarbaz
    +     )"
    > cx <- 0 +
    +       0i
    >     # comment
    > cx1 <- 1 + # comment
    +       1i   # comment
    > # comment
    > #
    > 
    > # make sure guild can handle multiple globals defined on one line
    > # here, b2, s4, and s5 are flags, not_a_global and b2 are not
    > b2 <- TRUE; s4 = "A string"; not_a_global <- b2; b2; s5 <- "Another string";
    [1] TRUE
    > # guild should always modify just the first global flag assignment location
    > duplicated_flag = 1L
    > duplicated_flag = -1L
    > stopifnot(identical(duplicated_flag, -1L))
    > # complex flags are a little tricky, extra tests:
    > cx2 <- 2+2i
    > cx3 <- 3 + 3i
    > globals <- mget(ls())
    > i2 <- - 123L # test token substitution on literals that
    > i3 <- + 123L # are really unary calls of `-` or `+`
    > {
    +   cat("R exprs:\n")
    +   for (nm in names(globals))
    +     cat("  ", nm, " = ", deparse1(get(nm)), "\n", sep = "")
    + }
    R exprs:
      b = FALSE
      b2 = TRUE
      cx = 0+0i
      cx1 = 1+1i
      cx2 = 2+2i
      cx3 = 3+3i
      duplicated_flag = -1L
      f = 1.123
      i = 123L
      not_a_global = TRUE
      s = "Howdy Guild"
      s2 = "foo"
      s3 = "\n    foobarbaz\n    "
      s4 = "A string"
      s5 = "Another string"
    > # echo can be turned off mid-run
    > # supports the same behavior as `R -f file.R` at the command line
    > options(echo = FALSE)
    YAML:
      b: no
      b2: yes
      cx: 0+0i
      cx1: 1+1i
      cx2: 2+2i
      cx3: 3+3i
      duplicated_flag: -1
      f: 1.123
      i: 123
      not_a_global: yes
      s: Howdy Guild
      s2: foo
      s3: "\n    foobarbaz\n    "
      s4: A string
      s5: Another string

---

    > #| echo: true
    > 
    > i <- 456L
    > f <- 4.56
    > s <- "Howdy Back"
    > # make sure guild can handle expressions that span more than one line, or
    > # have loitering comments, or odd whitespace indentation, or complex
    > # multiline string literals
    >   b <-
    +   # a comment in the middle
    +   TRUE # a trailing comment
    > # a comment after
    > #
    > #
    >     s2 <- "abc"
    >       s3 <- "def"
    > cx <- 1 +
    +       1i
    >     # comment
    > cx1 <- 2 + # comment
    +       2i   # comment
    > # comment
    > #
    > 
    > # make sure guild can handle multiple globals defined on one line
    > # here, b2, s4, and s5 are flags, not_a_global and b2 are not
    > b2 <- TRUE; s4 = "A loooooooonger string"; not_a_global <- b2; b2; s5 <- "a different string";
    [1] TRUE
    > # guild should always modify just the first global flag assignment location
    > duplicated_flag = 99L
    > duplicated_flag = -1L
    > stopifnot(identical(duplicated_flag, -1L))
    > # complex flags are a little tricky, extra tests:
    > cx2 <- 22+22i
    > cx3 <- 33+33i
    > globals <- mget(ls())
    > i2 <- + 123L # test token substitution on literals that
    > i3 <- - 123L # are really unary calls of `-` or `+`
    > {
    +   cat("R exprs:\n")
    +   for (nm in names(globals))
    +     cat("  ", nm, " = ", deparse1(get(nm)), "\n", sep = "")
    + }
    R exprs:
      b = TRUE
      b2 = TRUE
      cx = 1+1i
      cx1 = 2+2i
      cx2 = 22+22i
      cx3 = 33+33i
      duplicated_flag = -1L
      f = 4.56
      i = 456L
      not_a_global = TRUE
      s = "Howdy Back"
      s2 = "abc"
      s3 = "def"
      s4 = "A loooooooonger string"
      s5 = "a different string"
    > # echo can be turned off mid-run
    > # supports the same behavior as `R -f file.R` at the command line
    > options(echo = FALSE)
    YAML:
      b: yes
      b2: yes
      cx: 1+1i
      cx1: 2+2i
      cx2: 22+22i
      cx3: 33+33i
      duplicated_flag: -1
      f: 4.56
      i: 456
      not_a_global: yes
      s: Howdy Back
      s2: abc
      s3: def
      s4: A loooooooonger string
      s5: a different string

---

    > #| echo: true
    > 
    > i <- 123L
    > f <- 1.123
    > s <- "foo\nbar\nbaz"
    > # make sure guild can handle expressions that span more than one line, or
    > # have loitering comments, or odd whitespace indentation, or complex
    > # multiline string literals
    >   b <-
    +   # a comment in the middle
    +   FALSE # a trailing comment
    > # a comment after
    > #
    > #
    >     s2 <- r"(foo)"
    >       s3 <- r"(
    +     foobarbaz
    +     )"
    > cx <- 0 +
    +       0i
    >     # comment
    > cx1 <- 1 + # comment
    +       1i   # comment
    > # comment
    > #
    > 
    > # make sure guild can handle multiple globals defined on one line
    > # here, b2, s4, and s5 are flags, not_a_global and b2 are not
    > b2 <- TRUE; s4 = "s"; not_a_global <- b2; b2; s5 <- "Another string";
    [1] TRUE
    > # guild should always modify just the first global flag assignment location
    > duplicated_flag = 1L
    > duplicated_flag = -1L
    > stopifnot(identical(duplicated_flag, -1L))
    > # complex flags are a little tricky, extra tests:
    > cx2 <- 2+2i
    > cx3 <- 3 + 3i
    > globals <- mget(ls())
    > i2 <- - 123L # test token substitution on literals that
    > i3 <- + 123L # are really unary calls of `-` or `+`
    > {
    +   cat("R exprs:\n")
    +   for (nm in names(globals))
    +     cat("  ", nm, " = ", deparse1(get(nm)), "\n", sep = "")
    + }
    R exprs:
      b = FALSE
      b2 = TRUE
      cx = 0+0i
      cx1 = 1+1i
      cx2 = 2+2i
      cx3 = 3+3i
      duplicated_flag = -1L
      f = 1.123
      i = 123L
      not_a_global = TRUE
      s = "foo\nbar\nbaz"
      s2 = "foo"
      s3 = "\n    foobarbaz\n    "
      s4 = "s"
      s5 = "Another string"
    > # echo can be turned off mid-run
    > # supports the same behavior as `R -f file.R` at the command line
    > options(echo = FALSE)
    YAML:
      b: no
      b2: yes
      cx: 0+0i
      cx1: 1+1i
      cx2: 2+2i
      cx3: 3+3i
      duplicated_flag: -1
      f: 1.123
      i: 123
      not_a_global: yes
      s: |-
        foo
        bar
        baz
      s2: foo
      s3: "\n    foobarbaz\n    "
      s4: s
      s5: Another string

# guild run w/ flags-dest: globals; R < 4.0

    > #| echo: true
    > 
    > i <- 123L
    > f <- 1.123
    > s <- "Howdy Guild"
    > # make sure guild can handle expressions that span more than one line, or
    > # have loitering comments, or odd whitespace indentation, or complex
    > # multiline string literals
    >   b <-
    +   # a comment in the middle
    +   FALSE # a trailing comment
    > # a comment after
    > #
    > #
    >     s2 <- "foo"
    >       s3 <- "
    +     foobarbaz
    +     "
    > cx <- 0 +
    +       0i
    >     # comment
    > cx1 <- 1 + # comment
    +       1i   # comment
    > # comment
    > #
    > 
    > # make sure guild can handle multiple globals defined on one line
    > # here, b2, s4, and s5 are flags, not_a_global and b2 are not
    > b2 <- TRUE; s4 = "A string"; not_a_global <- b2; b2; s5 <- "Another string";
    [1] TRUE
    > # guild should always modify just the first global flag assignment location
    > duplicated_flag = 1L
    > duplicated_flag = -1L
    > stopifnot(identical(duplicated_flag, -1L))
    > # complex flags are a little tricky, extra tests:
    > cx2 <- 2+2i
    > cx3 <- 3 + 3i
    > globals <- mget(ls())
    > i2 <- - 123L # test token substitution on literals that
    > i3 <- + 123L # are really unary calls of `-` or `+`
    > {
    +   cat("R exprs:\n")
    +   for (nm in names(globals))
    +     cat("  ", nm, " = ", deparse(get(nm)), "\n", sep = "")
    + }
    R exprs:
      b = FALSE
      b2 = TRUE
      cx = 0+0i
      cx1 = 1+1i
      cx2 = 2+2i
      cx3 = 3+3i
      duplicated_flag = -1L
      f = 1.123
      i = 123L
      not_a_global = TRUE
      s = "Howdy Guild"
      s2 = "foo"
      s3 = "\n    foobarbaz\n    "
      s4 = "A string"
      s5 = "Another string"
    > # echo can be turned off mid-run
    > # supports the same behavior as `R -f file.R` at the command line
    > options(echo = FALSE)
    YAML:
      b: no
      b2: yes
      cx: 0+0i
      cx1: 1+1i
      cx2: 2+2i
      cx3: 3+3i
      duplicated_flag: -1
      f: 1.123
      i: 123
      not_a_global: yes
      s: Howdy Guild
      s2: foo
      s3: "\n    foobarbaz\n    "
      s4: A string
      s5: Another string

---

    > #| echo: true
    > 
    > i <- 456L
    > f <- 4.56
    > s <- "Howdy Back"
    > # make sure guild can handle expressions that span more than one line, or
    > # have loitering comments, or odd whitespace indentation, or complex
    > # multiline string literals
    >   b <-
    +   # a comment in the middle
    +   TRUE # a trailing comment
    > # a comment after
    > #
    > #
    >     s2 <- "abc"
    >       s3 <- "def"
    > cx <- 1 +
    +       1i
    >     # comment
    > cx1 <- 2 + # comment
    +       2i   # comment
    > # comment
    > #
    > 
    > # make sure guild can handle multiple globals defined on one line
    > # here, b2, s4, and s5 are flags, not_a_global and b2 are not
    > b2 <- TRUE; s4 = "A loooooooonger string"; not_a_global <- b2; b2; s5 <- "a different string";
    [1] TRUE
    > # guild should always modify just the first global flag assignment location
    > duplicated_flag = 99L
    > duplicated_flag = -1L
    > stopifnot(identical(duplicated_flag, -1L))
    > # complex flags are a little tricky, extra tests:
    > cx2 <- 22+22i
    > cx3 <- 33+33i
    > globals <- mget(ls())
    > i2 <- + 123L # test token substitution on literals that
    > i3 <- - 123L # are really unary calls of `-` or `+`
    > {
    +   cat("R exprs:\n")
    +   for (nm in names(globals))
    +     cat("  ", nm, " = ", deparse(get(nm)), "\n", sep = "")
    + }
    R exprs:
      b = TRUE
      b2 = TRUE
      cx = 1+1i
      cx1 = 2+2i
      cx2 = 22+22i
      cx3 = 33+33i
      duplicated_flag = -1L
      f = 4.56
      i = 456L
      not_a_global = TRUE
      s = "Howdy Back"
      s2 = "abc"
      s3 = "def"
      s4 = "A loooooooonger string"
      s5 = "a different string"
    > # echo can be turned off mid-run
    > # supports the same behavior as `R -f file.R` at the command line
    > options(echo = FALSE)
    YAML:
      b: yes
      b2: yes
      cx: 1+1i
      cx1: 2+2i
      cx2: 22+22i
      cx3: 33+33i
      duplicated_flag: -1
      f: 4.56
      i: 456
      not_a_global: yes
      s: Howdy Back
      s2: abc
      s3: def
      s4: A loooooooonger string
      s5: a different string

---

    > #| echo: true
    > 
    > i <- 123L
    > f <- 1.123
    > s <- "foo\nbar\nbaz"
    > # make sure guild can handle expressions that span more than one line, or
    > # have loitering comments, or odd whitespace indentation, or complex
    > # multiline string literals
    >   b <-
    +   # a comment in the middle
    +   FALSE # a trailing comment
    > # a comment after
    > #
    > #
    >     s2 <- "foo"
    >       s3 <- "
    +     foobarbaz
    +     "
    > cx <- 0 +
    +       0i
    >     # comment
    > cx1 <- 1 + # comment
    +       1i   # comment
    > # comment
    > #
    > 
    > # make sure guild can handle multiple globals defined on one line
    > # here, b2, s4, and s5 are flags, not_a_global and b2 are not
    > b2 <- TRUE; s4 = "s"; not_a_global <- b2; b2; s5 <- "Another string";
    [1] TRUE
    > # guild should always modify just the first global flag assignment location
    > duplicated_flag = 1L
    > duplicated_flag = -1L
    > stopifnot(identical(duplicated_flag, -1L))
    > # complex flags are a little tricky, extra tests:
    > cx2 <- 2+2i
    > cx3 <- 3 + 3i
    > globals <- mget(ls())
    > i2 <- - 123L # test token substitution on literals that
    > i3 <- + 123L # are really unary calls of `-` or `+`
    > {
    +   cat("R exprs:\n")
    +   for (nm in names(globals))
    +     cat("  ", nm, " = ", deparse(get(nm)), "\n", sep = "")
    + }
    R exprs:
      b = FALSE
      b2 = TRUE
      cx = 0+0i
      cx1 = 1+1i
      cx2 = 2+2i
      cx3 = 3+3i
      duplicated_flag = -1L
      f = 1.123
      i = 123L
      not_a_global = TRUE
      s = "foo\nbar\nbaz"
      s2 = "foo"
      s3 = "\n    foobarbaz\n    "
      s4 = "s"
      s5 = "Another string"
    > # echo can be turned off mid-run
    > # supports the same behavior as `R -f file.R` at the command line
    > options(echo = FALSE)
    YAML:
      b: no
      b2: yes
      cx: 0+0i
      cx1: 1+1i
      cx2: 2+2i
      cx3: 3+3i
      duplicated_flag: -1
      f: 1.123
      i: 123
      not_a_global: yes
      s: |-
        foo
        bar
        baz
      s2: foo
      s3: "\n    foobarbaz\n    "
      s4: s
      s5: Another string

# flag names that need quoting work

    > # some yaml keys need to be quoted, otherwise...
    > n    <- 1L   # bool
    > no   <- 2L   # bool
    > y    <- 3L   # bool
    > yes  <- 4L   # bool
    > .n   <- 5L   # invalid prefix
    > .no  <- 6L   # invalid prefix
    > `_x` <- 7L   # invalid prefix
    > `_y` <- 9L   # invalid prefix

---

    > # some yaml keys need to be quoted, otherwise...
    > n    <- 101L   # bool
    > no   <- 102L   # bool
    > y    <- 103L   # bool
    > yes  <- 104L   # bool
    > .n   <- 105L   # invalid prefix
    > .no  <- 106L   # invalid prefix
    > `_x` <- 107L   # invalid prefix
    > `_y` <- 109L   # invalid prefix

