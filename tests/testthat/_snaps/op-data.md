# guild run w/ flags-dest: globals

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

    Replaced expression 'i <- 123L' on line 3 with 'i <- 456L'
    Replaced expression 'f <- 1.123' on line 4 with 'f <- 4.56'
    Replaced expression 's <- "Howdy Guild"' on line 5 with 's <- "Howdy Back"'
    Replaced expression 'b <- FALSE' on line 10 with 'b <- TRUE'
    Replaced expression 's2 <- "foo"' on line 16 with 's2 <- "abc"'
    Replaced expression 's3 <- "\n    foobarbaz\n    "' on line 17 with 's3 <- "def"'
    Replaced expression 'cx <- 0+0i' on line 20 with 'cx <- 1+1i'
    Replaced expression 'cx1 <- 1+1i' on line 24 with 'cx1 <- 2+2i'
    Replaced expression 's4 = "A string"' on line 31 with 's4 = "A loooooooonger string"'
    Replaced expression 's5 <- "Another string"' on line 31 with 's5 <- "a different string"'
    Replaced expression 'duplicated_flag = 1L' on line 35 with 'duplicated_flag = 99L'
    Replaced expression 'cx2 <- 2+2i' on line 40 with 'cx2 <- 22+22i'
    Replaced expression 'cx3 <- 3+3i' on line 41 with 'cx3 <- 33+33i'
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
    YAML:
      b: yes
      b2: yes
      cx: 1+1i
      cx1: 2+2i
      cx2: 22+22i
      cx3: 33+33i
      duplicated_flag: -1
      f: 4.5599999999999996
      i: 456
      not_a_global: yes
      s: Howdy Back
      s2: abc
      s3: def
      s4: A loooooooonger string
      s5: a different string

---

    Replaced expression 's <- "Howdy Guild"' on line 5 with 's <- "foo\nbar\nbaz"'
    Replaced expression 's4 = "A string"' on line 31 with 's4 = "s"'
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

