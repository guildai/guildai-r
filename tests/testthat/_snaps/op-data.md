# guild run w/ flags-dest: globals

    list(b = FALSE, cx = 0+0i, cx1 = 1+1i, f = 1.123, i = 123L, s = "Howdy Guild", 
        s2 = "foo", s3 = "\n    foobarbaz\n    ")
    b: no
    cx: 0+0i
    cx1: 1+1i
    f: 1.123
    i: 123
    s: Howdy Guild
    s2: foo
    s3: "\n    foobarbaz\n    "

---

    Replacing expression 'i <- 123L' on line 3 with 'i <- 456L'
    Replacing expression 'f <- 1.123' on line 4 with 'f <- 4.56'
    Replacing expression 's <- "Howdy Guild"' on line 5 with 's <- "Howdy Back"'
    Replacing expression 'b <- FALSE' on line 6 with 'b <- TRUE'
    Replacing expression 's2 <- "foo"' on line 12 with 's2 <- "abc"'
    Replacing expression 's3 <- "\n    foobarbaz\n    "' on line 13 with 's3 <- "def"'
    Replacing expression 'cx <- 0+0i' on line 16 with 'cx <- 1+1i'
    Replacing expression 'cx1 <- 1+1i' on line 20 with 'cx1 <- 2+2i'
    list(b = TRUE, cx = 2, cx1 = 4, f = 4.56, i = 456L, s = "Howdy Back", 
        s2 = "abc", s3 = "def")
    b: yes
    cx: 2.0
    cx1: 4.0
    f: 4.5599999999999996
    i: 456
    s: Howdy Back
    s2: abc
    s3: def

---

    Replacing expression 's <- "Howdy Guild"' on line 5 with 's <- "foo\nbar\nbaz"'
    list(b = FALSE, cx = 0+0i, cx1 = 1+1i, f = 1.123, i = 123L, s = "foo\nbar\nbaz", 
        s2 = "foo", s3 = "\n    foobarbaz\n    ")
    b: no
    cx: 0+0i
    cx1: 1+1i
    f: 1.123
    i: 123
    s: |-
      foo
      bar
      baz
    s2: foo
    s3: "\n    foobarbaz\n    "

