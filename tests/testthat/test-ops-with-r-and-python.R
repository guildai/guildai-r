

test_that("VIRTUAL_ENV and other python cruft doesn't interfere with guild", {

  local_project(test_resource("use-python.R"))
  # browser(); Sys.setenv(GUILD_DEBUG_R=1)

  if(!nzchar(Sys.which("python")) && !nzchar(guildai:::find_python()))
      withr::local_path(guildai:::find_python())

  if(!nzchar(Sys.which("python")) && !nzchar(Sys.which("python3")))
    skip("no python on PATH")

  expect_single_run_succeedes <- function(info = NULL) {
    guild_run("use-python.R")
    expect_identical(runs_info(1L)$exit_status, 0L,
                     info = info)
    guild("runs delete --yes", stderr = FALSE)
  }

  expect_batch_run_succeedes <- function(info = NULL) {
    guild_run("use-python.R", flags = list(n = 1:3))
    expect_identical(runs_info(1:3)$exit_status, c(0L, 0L, 0L),
                     info = info)
    guild("runs delete --yes", stderr = FALSE)
  }

  do_test_batch <- TRUE
  expect_run_succeedes <- function(info) {
    expect_single_run_succeedes(info)
    if(do_test_batch)
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

  # do_test_batch <- is_windows() || tryCatch(local({
  #   # ._pth files only work correctly in Python >= 3.11 on non-Windows
  #   shebang <- readLines(guildai:::find_guild(), 1)
  #   python <- str_drop_prefix(shebang, "#!")
  #   py_ver <- system(paste(python, "--version"), intern = TRUE)
  #   py_ver <- numeric_version(str_drop_prefix(py_ver, "Python "))
  #   isTRUE(py_ver >= "3.11")
  # }), error = function(e) TRUE)

  expect_run_succeedes("presence of things that look like modules in cwd")

  # Test presence of bad/wrong modules on PYTHONPATH
  with_envvar(c("PYTHONPATH" = normalizePath(getwd())), {
    expect_run_succeedes("PYTHONPATH with guild incompatible modules")
  })

  # Test user activated virtual_env doesn't interfere w/
  # direct call to guild executable
  python <- Sys.which("python")
  if(!nzchar(python)) python <- Sys.which("python3")
  system2(python, c("-m", "venv", "my-venv"))
  file.create("my-venv/.guild-nocopy")

  # "activate" the venv.
  bin_path <- if(is_windows()) "my-venv/Scripts" else "my-venv/bin"
  local_envvar(VIRTUAL_ENV = normalizePath(file.path(getwd(), "my-venv")))
  local_path(normalizePath(bin_path))

  expect_run_succeedes("activated VIRTUAL_ENV")

  with_envvar(c("PYTHONPATH" = normalizePath(getwd())), {
    expect_run_succeedes("PYTHONPATH set")
  })

  # if(!do_test_batch)
  #   skip("user set PYTHON* vars leak into guild_python_exe runtime on this platform")

  # skip("user set PYTHONHOME leaks into guild_python_exe runtime")
  # local_envvar(PYTHONHOME = Sys.getenv("VIRTUAL_ENV"))
  # expect_run_succeedes("PYTHONHOME set")

})
