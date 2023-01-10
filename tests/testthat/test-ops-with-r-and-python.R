
# TODO: should flags go in ... ? e.g. guild_run(op, flagx = 1, flagy = 2)?

test_that("VIRTUAL_ENV and other python cruft doesn't interfere with guild", {

  local_project(test_resource("use-python.R"))
  # browser(); Sys.setenv(DEBUGR=1)

  expect_run_succeedes <- function(info = NULL) {
    # single run
    guild_run("use-python.R")
    expect_identical(runs_info(1L)$exit_status, 0L,
                     info = info)

    # batch run
    guild_run("use-python.R", flags = list(n = 1:3))
    expect_identical(runs_info(1:3)$exit_status, c(0L, 0L, 0L),
                     info = info)
  }

  expect_run_succeedes("base case")

  # test `guild run` succeeds in presence of things that could be
  # misinterpreted by guild executable's python session as modules to be imported
  dir.create("click")
  for(fi in c("numpy.py", "__init__.py", "click.py", "guild.py")) {
    writeLines('raise Exception("I should not be evaluated")', fi)
    writeLines('raise Exception("I should not be evaluated")', file.path("click", fi))
  }

  expect_run_succeedes("presence of things that look like modules in cwd")

  # Test presence of bad/wrong modules on PYTHONPATH
  with_envvar(c("PYTHONPATH" = getwd()), {
    expect_run_succeedes("PYTHONPATH with guild incompatible modules")
  })

  # Test user activated virtual_env doesn't interfere w/
  # direct call to guild executable
  if (is_windows()) {
    skip("Not yet implemented")

  } else { # mac, linux
    system2("python3", c("-m", "venv", "my-venv"))
    local_envvar(VIRTUAL_ENV = file.path(getwd(), "my-venv"))
    local_path(file.path(getwd(), "myvenv/bin"))

    expect_run_succeedes("activated VIRTUAL_ENV")

    local_envvar(PYTHONHOME = Sys.getenv(VIRTUAL_ENV))

    expect_run_succeedes("PYTHONHOME set")
  }


})

