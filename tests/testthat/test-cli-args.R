test_that("as_cli_args works", {

  expect_equal(unclass(as_cli_args(foo = TRUE)),  "--foo")
  expect_equal(unclass(as_cli_args(foo = FALSE)), "--not-foo")
  expect_length(unclass(as_cli_args(foo = NA)), 0)


  expect_equal(unclass(as_cli_args(foo = "a")),  c("--foo", "a"))
  expect_equal(unclass(as_cli_args(foo = c("a", "b"))), c("--foo", "a", "--foo", "b"))
  expect_length(unclass(as_cli_args(foo = NULL)), 0)

})
