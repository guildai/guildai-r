
# TODO: should flags go in ... ? e.g. guild_run(op, flagx = 1, flagy = 2)?

test_that("VIRTUAL_ENV and other python cruft doesn't interfere with guild", {

  local_project(test_resource("use-python.R"))
   # browser(); Sys.setenv(DEBUGR=1)

  if(!nzchar(Sys.which("python")) && !nzchar(guildai:::find_python()))
    withr::local_path(guildai:::find_python())

  skip_if(!nzchar(Sys.which("python")))

  expect_single_run_succeedes <- function(info = NULL) {
    guild_run("use-python.R")
    expect_identical(runs_info(1L)$exit_status, 0L,
                     info = info)
    runs_delete()
  }

  expect_batch_run_succeedes <- function(info = NULL) {
    guild_run("use-python.R", flags = list(n = 1:3))
    expect_identical(runs_info(1:3)$exit_status, c(0L, 0L, 0L),
                     info = info)
    runs_delete()
  }

  expect_run_succeedes <- function(info) {
    expect_single_run_succeedes(info)
    expect_batch_run_succeedes(info)
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
  system2("python", c("-m", "venv", "my-venv"))

  # "activate" the venv.
  bin_path <- if(is_windows()) "my-venv/Scripts" else "my-venv/bin"
  local_envvar(VIRTUAL_ENV = normalizePath(file.path(getwd(), "my-venv")))
  local_path(normalizePath(bin_path))

  expect_run_succeedes("activated VIRTUAL_ENV")

  with_envvar(c("PYTHONPATH" = normalizePath(getwd())), {
    expect_run_succeedes("PYTHONPATH set")
  })


  # skip("user set PYTHONHOME leaks into guild_python_exe runtime")
  # local_envvar(PYTHONHOME = Sys.getenv("VIRTUAL_ENV"))
  # expect_run_succeedes("PYTHONHOME set")

})

