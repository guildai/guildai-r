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

