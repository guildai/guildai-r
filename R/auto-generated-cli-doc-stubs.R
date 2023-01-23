#' guild_run_cli
#' Run an operation.
#'
#' By default Guild tries to run `OPERATION` for the default model
#' defined in the current project.
#'
#' If `MODEL` is specified, Guild uses it instead of the default
#' model.
#'
#' `OPERATION` may alternatively be a Python script. In this case any
#' current project is ignored and the script is run directly. Options
#' in the format ``--NAME=VAL`` can be passed to the script using
#' flags (see below).
#'
#' `[MODEL]:OPERATION` may be omitted if `restart` or `proto` is
#' specified, in which case the operation used in `RUN` is used.
#'
#' Specify `FLAG` values in the form `FLAG=VAL`.
#'
#' ### Batch Files
#'
#' One or more batch files can be used to run multiple trials by
#' specifying the file path as `@PATH`.
#'
#' For example, to run trials specified in a CSV file named
#' `trials.csv`, run:
#'
#'     guild run [MODEL:]OPERATION @trials.csv
#'
#' NOTE: At this time you must specify the operation with batch files
#' - batch files only contain flag values and cannot be used to run
#' different operations for the same command.
#'
#' Batch files may be formatted as CSV, JSON, or YAML. Format is
#' determined by the file extension.
#'
#' Each entry in the file is used as a set of flags for a trial run.
#'
#' CSV files must have a header row containing the flag names. Each
#' subsequent row is a corresponding list of flag values that Guild
#' uses for a generated trial.
#'
#' JSON and YAML files must contain a top-level list of flag-to-value
#' maps.
#'
#' Use `print_trials` to preview the trials run for the specified
#' batch files.
#'
#' ### Flag Lists
#'
#' A list of flag values may be specified using the syntax
#' `[VAL1[,VAL2]...]`. Lists containing white space must be
#' quoted. When a list of values is provided, Guild generates a trial
#' run for each value. When multiple flags have list values, Guild
#' generates the cartesian product of all possible flag combinations.
#'
#' Flag lists may be used to perform grid search operations.
#'
#' For example, the following generates four runs for operation
#' `train` and flags `learning-rate` and `batch-size`:
#'
#'     guild run train learning-rate[0.01,0.1] batch-size=[10,100]
#'
#' You can preview the trials generated from flag lists using
#' `print_trials`. You can save the generated trials to a batch
#' file using `save_trials`. For more information, see PREVIEWING
#' AND SAVING TRIALS below.
#'
#' When `optimizer` is specified, flag lists may take on different
#' meaning depending on the type of optimizer. For example, the
#' `random` optimizer randomly selects values from a flag list,
#' rather than generate trials for each value. See OPTIMIZERS for
#' more information.
#'
#' ### Optimizers
#'
#' A run may be optimized using `optimizer`. An optimizer runs up
#' to `max_trials` runs using flag values and flag configuration.
#'
#' For details on available optimizers and their behavior, refer to
#' https://guild.ai/optimizers/.
#'
#' ### Limit Trials
#'
#' When using flag lists or optimizers, which generate trials, you
#' can limit the number of trials with `max_trials`. By default,
#' Guild limits the number of generated trials to 20.
#'
#' Guild limits trials by randomly sampling the maximum number from
#' the total list of generated files. You can specify the seed used
#' for the random sample with `random_seed`. The random seed is
#' guaranteed to generate consistent results when used on the same
#' version of Python. When used across different versions of Python,
#' the results may be inconsistent.
#'
#' ### Preview or Save Trials
#'
#' When flag lists (used for grid search) or an optimizer is used,
#' you can preview the generated trials using `print_trials`. You
#' can save the generated trials as a CSV batch file using
#' `save_trials`.
#'
#' ### Start an Operation Using a Prototype Run
#'
#' If `proto` is specified, Guild applies the operation, flags, and
#' source code used in `RUN` to the new operation. You may add or
#' redefine flags in the new operation. You may use an alternative
#' operation, in which case only the flag values and source code from
#' `RUN` are applied. `RUN` must be a run ID or unique run ID prefix.
#'
#' ### Restart an Operation
#'
#' If `restart` is specified, `RUN` is restarted using its
#' operation and flags. Unlike `proto`, restart does not create a
#' new run. You cannot change the operation, flags, source code, or
#' run directory when restarting a run.
#'
#' ### Staging an Operation
#'
#' Use `stage` to stage an operation to be run later. Use `start`
#' with the staged run ID to start it.
#'
#' If `start` is specified, `RUN` is started using the same rules
#' applied to `restart` (see above).
#'
#' ### Alternate Run Directory
#'
#' To run an operation outside of Guild's run management facility,
#' use `run_dir` or `stage-dir` to specify an alternative run
#' directory. These options are useful when developing or debugging
#' an operation. Use `stage-dir` to prepare a run directory for an
#' operation without running the operation itself. This is useful
#' when you want to verify run directory layout or manually run an
#' operation in a prepared directory.
#'
#' **NOTE:** Runs started with `run_dir` are not visible to Guild
#' and do not appear in run listings.
#'
#' ### Control Visible GPUs
#'
#' By default, operations have access to all available GPU
#' devices. To limit the GPU devices available to a run, use
#' `gpus`.
#'
#' For example, to limit visible GPU devices to `0` and `1`, run:
#'
#'     guild run gpus 0,1 ...
#'
#' To disable all available GPUs, use `no_gpus`.
#'
#' **NOTE:** `gpus` and `no_gpus` are used to construct the
#' `CUDA_VISIBLE_DEVICES` environment variable used for the run
#' process. If `CUDA_VISIBLE_DEVICES` is set, using either of these
#' options redefines that environment variable for the run.
#'
#' ### Optimize Runs
#'
#' Use `optimizer` to run the operation multiple times in attempt
#' to optimize a result. Use `minimize` or `maximize` to indicate
#' what should be optimized. Use `--max-runs` to indicate the maximum
#' number of runs the optimizer should generate.
#'
#' ### Edit Flags
#'
#' Use `edit_flags` to use an editor to review and modify flag
#' values. Guild uses the editor defined in `VISUAL` or `EDITOR`
#' environment variables. If neither environment variable is defined,
#' Guild uses an editor suitable for the current platform.
#'
#' ### Debug Source Code
#'
#' Use `debug_sourcecode` to specify the location of project source
#' code for debugging. Guild uses this path instead of the location
#' of the copied soure code for the run. For example, when debugging
#' project files, use this option to ensure that modules are loaded
#' from the project location rather than the run directory.
#'
#' ### Breakpoints
#'
#' Use `break` to set breakpoints for Python based operations.
#' `LOCATION` may be specified as `[FILENAME:]LINE` or as
#' `MODULE.FUNCTION`.
#'
#' If `FILENAME` is not specified, the main module is assumed. Use
#' the value ``1`` to break at the start of the main module (line 1).
#'
#' Relative file names are resolved relative to the their location in
#' the Python system path. You can omit the `.py` extension.
#'
#' If a line number does not correspond to a valid breakpoint, Guild
#' attempts to set a breakpoint on the next valid breakpoint line in
#' the applicable module.
#'
#'
#' @param ... passed on to the `guild` executable. Arguments are automatically quoted with `shQuote()`, unless they are protected with `I()`. Pass `'--help'` or `help = TRUE` to see all options.
#' @param label Set a label for the run.
#' @param tag Associate TAG with run. May be used multiple times.
#' @param comment Comment associated with the run.
#' @param run_dir Use alternative run directory DIR. Cannot be used with `stage`.
#' @param stage (bool) Stage an operation.
#' @param start Start a staged run or restart an existing run. Cannot be used with `proto` or `run_dir`.
#' @param restart Start a staged run or restart an existing run. Cannot be used with `proto` or `run_dir`.
#' @param proto Use the operation, flags and source code from RUN. Flags may be added or redefined in this operation. Cannot be used with `restart`.
#' @param force_sourcecode (bool) Use working source code when `restart` or `proto` is specified. Ignored otherwise.
#' @param gpus Limit availabe GPUs to DEVICES, a comma separated list of device IDs. By default all GPUs are available. Cannot beused with `no_gpus`.
#' @param no_gpus (bool) Disable GPUs for run. Cannot be used with `gpus`.
#' @param batch_label Label to use for batch runs. Ignored for non-batch runs.
#' @param batch_tag Associate TAG with batch. Ignored for non-batch runs. May be used multiple times.
#' @param batch_comment Comment associated with batch.
#' @param optimizer Optimize the run using the specified algorithm. See Optimizing Runs for more information.
#' @param optimize (bool) Optimize the run using the default optimizer.
#' @param minimize Column to minimize when running with an optimizer. See help for compare command for details specifying a column. May not be used with `maximize`.
#' @param maximize Column to maximize when running with an optimizer. See help for compare command for details specifying a column. May not be used with `minimize`.
#' @param opt_flag Flag for OPTIMIZER. May be used multiple times.
#' @param max_trials Maximum number of trials to run in batch operations. Default is optimizer specific. If optimizer is not specified, default is 20.
#' @param trials Maximum number of trials to run in batch operations. Default is optimizer specific. If optimizer is not specified, default is 20.
#' @param stage_trials (bool) For batch operations, stage trials without running them.
#' @param remote Run the operation remotely.
#' @param force_flags (bool) Accept all flag assignments, even for undefined or invalid values.
#' @param force_deps (bool) Continue even when a required resource is not resolved.
#' @param stop_after Stop operation after N minutes.
#' @param fail_on_trial_error (bool) Stop batch operations when a trial exits with an error.
#' @param needed (bool) Run only if there is not an available matching run. A matching run is of the same operation with the same flag values that is not stopped due to an error.
#' @param background (bool) Run operation in background.
#' @param pidfile Run operation in background, writing the background process ID to PIDFILE.
#' @param no_wait (bool) Don't wait for a remote operation to complete. Ignored if run is local.
#' @param save_trials Saves generated trials to a CSV batch file. See BATCH FILES for more information.
#' @param keep_run (bool) Keep run even when configured with 'delete-on-success'.
#' @param keep_batch (bool) Keep batch run rather than delete it on success.
#' @param dep Include PATH as a dependency.
#' @param quiet (bool) Do not show output.
#' @param print_cmd (bool) Show operation command and exit.
#' @param print_env (bool) Show operation environment and exit.
#' @param print_trials (bool) Show generated trials and exit.
#' @param help_model (bool) Show model help and exit.
#' @param help_op (bool) Show operation help and exit.
#' @param test_output_scalars Test output scalars on output. Use '-' to read from standard intput.
#' @param test_sourcecode (bool) Test source code selection.
#' @param test_flags (bool) Test flag configuration.
#' @keywords internal
guild_run_cli <-
function (..., label = NULL, tag = NULL, comment = NULL, run_dir = NULL,
stage = NA, start = NULL, restart = NULL, proto = NULL, force_sourcecode = NA,
gpus = NULL, no_gpus = NA, batch_label = NULL, batch_tag = NULL,
batch_comment = NULL, optimizer = NULL, optimize = NA, minimize = NULL,
maximize = NULL, opt_flag = NULL, max_trials = NULL, trials = NULL,
stage_trials = NA, remote = NULL, force_flags = NA, force_deps = NA,
stop_after = NULL, fail_on_trial_error = NA, needed = NA,
background = NA, pidfile = NULL, no_wait = NA, save_trials = NULL,
keep_run = NA, keep_batch = NA, dep = NULL, quiet = NA, print_cmd = NA,
print_env = NA, print_trials = NA, help_model = NA, help_op = NA,
test_output_scalars = NULL, test_sourcecode = NA, test_flags = NA)
{
as_cli_args(as.list.environment(environment()), ...)
}

#' guild_api_runs_cli
#' Show runs as JSON.
#'
#' IMPORTANT: This command is experimental and subject to change without
#' notice.
#'
#'
#' @param ... passed on to the `guild` executable. Arguments are automatically quoted with `shQuote()`, unless they are protected with `I()`. Pass `'--help'` or `help = TRUE` to see all options.
#' @param format (bool) Format the JSON outout.
#' @param filter Filter runs using a filter expression. See Filter by Expression above for details.
#' @param operation Filter runs with operations matching `VAL`.
#' @param label Filter runs with labels matching VAL. To show unlabeled runs, use `unlabeled`.
#' @param unlabeled (bool) Filter runs without labels.
#' @param tag Filter runs with TAG.
#' @param comment Filter runs with comments matching VAL.
#' @param marked (bool) Filter marked runs.
#' @param unmarked (bool) Filter unmarked runs.
#' @param started Filter runs started within RANGE. See above for valid time ranges.
#' @param digest Filter runs with a matching source code digest.
#' @param running (bool) Filter runs that are still running.
#' @param completed (bool) Filter completed runs.
#' @param error (bool) Filter runs that exited with an error.
#' @param terminated (bool) Filter runs terminated by the user.
#' @param pending (bool) Filter pending runs.
#' @param staged (bool) Filter staged runs.
#' @param api_version API version.
#' @param deleted (bool) Show deleted runs.
#' @param include_batch (bool) Include batch runs.
#' @keywords internal
guild_api_runs_cli <-
function (..., format = NA, filter = NULL, operation = NULL,
label = NULL, unlabeled = NA, tag = NULL, comment = NULL,
marked = NA, unmarked = NA, started = NULL, digest = NULL,
running = NA, completed = NA, error = NA, terminated = NA,
pending = NA, staged = NA, api_version = NULL, deleted = NA,
include_batch = NA)
{
as_cli_args(as.list.environment(environment()), ...)
}

#' guild_view_cli
#' Visualize runs in a local web application.
#'
#' Features include:
#'
#'
#'   - View and filter runs
#'   - Compare runs
#'   - Browse run files
#'   - View run images and other media
#'   - View run output
#'
#' Guild View does not currently support starting or modifying
#' runs. For these operations, use the applicable command line
#' interface. Run 'guild help' for a complete list of commands.
#'
#' By default Guild View shows all runs. You can filter runs using
#' the command options described below.
#'
#' ### Specify Runs
#'
#' You may use one or more `RUN` arguments to indicate which runs
#' apply to the command. `RUN` may be a run ID, a run ID prefix, or a
#' one-based index corresponding to a run returned by the list
#' command.
#'
#' Indexes may also be specified in ranges in the form `START:END`
#' where `START` is the start index and `END` is the end
#' index. Either `START` or `END` may be omitted. If `START` is
#' omitted, all runs up to `END` are selected. If `END` id omitted,
#' all runs from `START` on are selected. If both `START` and `END`
#' are omitted (i.e. the ``:`` char is used by itself) all runs are
#' selected.
#'
#'
#'
#'
#' ### Filter by Operation
#'
#' Runs may be filtered by operation using `operation`.  A run is
#' only included if any part of its full operation name, including
#' the package and model name, matches the value.
#'
#' Use `operation` multiple times to include more runs.
#'
#' ### Filter by Label
#'
#' Use `label` to only include runs with labels containing a
#' specified value. To select runs that do not contain a label,
#' specify a dash '-' for `VAL`.
#'
#' Use `label` multiple times to include more runs.
#'
#' ### Filter by Tag
#'
#' Use `tag` to only include runs with a specified tag. Tags must
#' match completely and are case sensitive.
#'
#' Use `tag` multiple times to include more runs.
#'
#' ### Filter by Marked and Unmarked
#'
#' Use `marked` to only include marked runs.
#'
#' Use `unmarked` to only include unmarked runs. This option may
#' not be used with `marked`.
#'
#' ### Filter by Expression
#'
#' Use `filter` to limit runs that match a filter
#' expressions. Filter expressions compare run attributes, flag
#' values, or scalars to target values. They may include multiple
#' expressions with logical operators.
#'
#' For example, to match runs with flag `batch-size` equal to 100
#' that have `loss` less than 0.8, use:
#'
#'     filter 'batch-size = 10 and loss < 0.8'
#'
#' **IMPORTANT:** You must quote EXPR if it contains spaces or
#' characters that the shell uses (e.g. '<' or '>').
#'
#' Target values may be numbers, strings or lists containing numbers
#' and strings. Strings that contain spaces must be quoted, otherwise
#' a target string values does not require quotes. Lists are defined
#' using square braces where each item is separated by a comma.
#'
#' Comparisons may use the following operators: '=', '!=' (or '<>'),
#' '<', '<=', '>', '>='. Text comparisons may use 'contains' to test
#' for case-insensitive string membership. A value may be tested for
#' membership or not in a list using 'in' or 'not in'
#' respectively. An value may be tested for undefined using 'is
#' undefined' or defined using 'is not undefined'.
#'
#' Logical operators include 'or' and 'and'. An expression may be
#' negated by preceding it with 'not'. Parentheses may be used to
#' control the order of precedence when expressions are evaluated.
#'
#' If a value reference matches more than one type of run information
#' (e.g. a flag is named 'label', which is also a run attribute), the
#' value is read in order of run attribute, then flag value, then
#' scalar. To disambiguate the reference, use a prefix `attr:`,
#' `flag:`, or `scalar:` as needed. For example, to filter using a
#' flag value named 'label', use 'flag:label'.
#'
#' Other examples:
#'
#'
#'   `operation = train and acc > 0.9`
#'   `operation = train and (acc > 0.9 or loss < 0.3)`
#'   `batch-size = 100 or batch-size = 200`
#'   `batch-size in [100,200]`
#'   `batch-size not in [400,800]`
#'   `batch-size is undefined`
#'   `batch-size is not undefined`
#'   `label contains best and operation not in [test,deploy]`
#'   `status in [error,terminated]`
#'
#' **NOTE:** Comments and tags are not supported in filter
#' expressions at this time. Use `comment` and `tag` options
#' along with filter expressions to further refine a selection.
#'
#' ### Filter by Run Start Time
#'
#' Use `started` to limit runs to those that have started within a
#' specified time range.
#'
#' **IMPORTANT:** You must quote RANGE values that contain
#' spaces. For example, to filter runs started within the last hour,
#' use the option:
#'
#'     started 'last hour'
#'
#' You can specify a time range using several different forms:
#'
#'
#'   `after DATETIME`
#'   `before DATETIME`
#'   `between DATETIME and DATETIME`
#'   `last N minutes|hours|days`
#'   `today|yesterday`
#'   `this week|month|year`
#'   `last week|month|year`
#'   `N days|weeks|months|years ago`
#'
#' `DATETIME` may be specified as a date in the format ``YY-MM-DD``
#' (the leading ``YY-`` may be omitted) or as a time in the format
#' ``HH:MM`` (24 hour clock). A date and time may be specified
#' together as `DATE TIME`.
#'
#' When using ``between DATETIME and DATETIME``, values for
#' `DATETIME` may be specified in either order.
#'
#' When specifying values like ``minutes`` and ``hours`` the trailing
#' ``s`` may be omitted to improve readability. You may also use
#' ``min`` instead of ``minutes`` and ``hr`` instead of ``hours``.
#'
#' Examples:
#'
#'
#'   `after 7-1`
#'   `after 9:00`
#'   `between 1-1 and 4-30`
#'   `between 10:00 and 15:00`
#'   `last 30 min`
#'   `last 6 hours`
#'   `today`
#'   `this week`
#'   `last month`
#'   `3 weeks ago`
#'
#' ### Filter by Source Code Digest
#'
#' To show runs for a specific source code digest, use `-g` or
#' `digest` with a complete or partial digest value.
#'
#'
#' ### Filter by Run Status
#'
#' Runs may also be filtered by specifying one or more status
#' filters: `running`, `completed`, `error`, and
#' `terminated`. These may be used together to include runs that
#' match any of the filters. For example to only include runs that
#' were either terminated or exited with an error, use ``terminated
#' error``, or the short form ``-Set``.
#'
#' You may combine more than one status character with ``-S`` to
#' expand the filter. For example, ``-Set`` shows only runs with
#' terminated or error status.
#'
#' Status filters are applied before `RUN` indexes are resolved. For
#' example, a run index of ``1`` is the latest run that matches the
#' status filters.
#'
#'
#' @param ... passed on to the `guild` executable. Arguments are automatically quoted with `shQuote()`, unless they are protected with `I()`. Pass `'--help'` or `help = TRUE` to see all options.
#' @param host Name of host interface to listen on.
#' @param port Port to listen on.
#' @param include_batch (bool) Include batch runs.
#' @param no_open (bool) Don't open Guild View in a browser.
#' @param logging (bool) Log requests.
#' @param filter Filter runs using a filter expression. See Filter by Expression above for details.
#' @param operation Filter runs with operations matching `VAL`.
#' @param label Filter runs with labels matching VAL. To show unlabeled runs, use `unlabeled`.
#' @param unlabeled (bool) Filter runs without labels.
#' @param tag Filter runs with TAG.
#' @param comment Filter runs with comments matching VAL.
#' @param marked (bool) Filter marked runs.
#' @param unmarked (bool) Filter unmarked runs.
#' @param started Filter runs started within RANGE. See above for valid time ranges.
#' @param digest Filter runs with a matching source code digest.
#' @param running (bool) Filter runs that are still running.
#' @param completed (bool) Filter completed runs.
#' @param error (bool) Filter runs that exited with an error.
#' @param terminated (bool) Filter runs terminated by the user.
#' @param pending (bool) Filter pending runs.
#' @param staged (bool) Filter staged runs.
#' @keywords internal
guild_view_cli <-
function (..., host = NULL, port = NULL, include_batch = NA,
no_open = NA, logging = NA, filter = NULL, operation = NULL,
label = NULL, unlabeled = NA, tag = NULL, comment = NULL,
marked = NA, unmarked = NA, started = NULL, digest = NULL,
running = NA, completed = NA, error = NA, terminated = NA,
pending = NA, staged = NA)
{
as_cli_args(as.list.environment(environment()), ...)
}

#' guild_merge_cli
#' Copy run files to a project directory.
#'
#' By default, Guild copies run files into the current directory. To
#' copy files to a different directory, use ``target_dir DIR``.
#'
#' Guild checks that the run originated from the current directory
#' before copying files. If the run is associated with a project from
#' a different directory, or is from a package, Guild exits with an
#' error message. In this case, use `target_dir` to override the
#' check with an explicit path.
#'
#' The command fails if any file would be replaced, unless a) the
#' `replace` option is specified or b) the replaced file is
#' committed to the project VCS and unchanged. To prevent replacement
#' even when a file is committed to VCS and unchanged, specify
#' `no_replace`.
#'
#'
#' @param ... passed on to the `guild` executable. Arguments are automatically quoted with `shQuote()`, unless they are protected with `I()`. Pass `'--help'` or `help = TRUE` to see all options.
#' @param filter Filter runs using a filter expression. See Filter by Expression above for details.
#' @param operation Filter runs with operations matching `VAL`.
#' @param label Filter runs with labels matching VAL. To show unlabeled runs, use `unlabeled`.
#' @param unlabeled (bool) Filter runs without labels.
#' @param tag Filter runs with TAG.
#' @param comment Filter runs with comments matching VAL.
#' @param marked (bool) Filter marked runs.
#' @param unmarked (bool) Filter unmarked runs.
#' @param started Filter runs started within RANGE. See above for valid time ranges.
#' @param digest Filter runs with a matching source code digest.
#' @param running (bool) Filter runs that are still running.
#' @param completed (bool) Filter completed runs.
#' @param error (bool) Filter runs that exited with an error.
#' @param terminated (bool) Filter runs terminated by the user.
#' @param pending (bool) Filter pending runs.
#' @param staged (bool) Filter staged runs.
#' @param target_dir Directory to merge run files to (required if project directory cannot be determined for the run).
#' @param sourcecode (bool) Only copy run source code. Implies use of ``skip_deps``. Cannot be used with ``skip_sourcecode``.
#' @param all (bool) Copy all run files. May be used with ``skip_sourcecode``, ``skip_deps``, and ``exclude`` to copy all but the skipped/excluded files.
#' @param skip_sourcecode (bool) Don't copy run source code.
#' @param skip_deps (bool) Don't copy project-local dependencies.
#' @param exclude Exclude a file or pattern (may be used multiple times).
#' @param no_summary (bool) Don't generate a run summary.
#' @param summary_name Name used for the run summary. Use '${run_id}' in the name to include the run ID.
#' @param preview (bool) Show what would happen on a merge.
#' @param replace (bool) Allow replacement of existing files. Cannot be used with `no_replace`
#' @param no_replace (bool) Fail if any target file would be replaced, even if that file is committed to the project VCS. Cannot be used with ``replace``.
#' @keywords internal
guild_merge_cli <-
function (..., filter = NULL, operation = NULL, label = NULL,
unlabeled = NA, tag = NULL, comment = NULL, marked = NA,
unmarked = NA, started = NULL, digest = NULL, running = NA,
completed = NA, error = NA, terminated = NA, pending = NA,
staged = NA, target_dir = NULL, sourcecode = NA, all = NA,
skip_sourcecode = NA, skip_deps = NA, exclude = NULL, no_summary = NA,
summary_name = NULL, preview = NA, replace = NA, no_replace = NA)
{
as_cli_args(as.list.environment(environment()), ...)
}

#' guild_select_cli
#' Select a run and shows its ID.
#'
#' This command is generally used when specifying a run ID for
#' another Guild command. For example, to restart the latest `train`
#' run:
#'
#'     `guild run --restart $(guild select -o train)`
#'
#' ### Specify a Run
#'
#' You may specify a run using a run ID, a run ID prefix, or a
#' one-based index corresponding to a run returned by the `list`
#' command.
#'
#'
#'
#' If RUN isn't specified, the latest matching run is selected.
#'
#' ### Selecting Min or Max Scalar
#'
#' To select the run with the lowest or highest column value, use
#' `min` or `max` respectively. For example, to select the run
#' with the lowest `loss` scalar value, use `min loss`.
#'
#' For help with COLSPEC formatting, see `COLUMN SPECS` in `compare`
#' help by running `guild compare help`.
#'
#' Other run filters are applied before selecting a minimum or
#' maximium scalar value.
#'
#'
#' ### Filter by Operation
#'
#' Runs may be filtered by operation using `operation`.  A run is
#' only included if any part of its full operation name, including
#' the package and model name, matches the value.
#'
#' Use `operation` multiple times to include more runs.
#'
#' ### Filter by Label
#'
#' Use `label` to only include runs with labels containing a
#' specified value. To select runs that do not contain a label,
#' specify a dash '-' for `VAL`.
#'
#' Use `label` multiple times to include more runs.
#'
#' ### Filter by Tag
#'
#' Use `tag` to only include runs with a specified tag. Tags must
#' match completely and are case sensitive.
#'
#' Use `tag` multiple times to include more runs.
#'
#' ### Filter by Marked and Unmarked
#'
#' Use `marked` to only include marked runs.
#'
#' Use `unmarked` to only include unmarked runs. This option may
#' not be used with `marked`.
#'
#' ### Filter by Expression
#'
#' Use `filter` to limit runs that match a filter
#' expressions. Filter expressions compare run attributes, flag
#' values, or scalars to target values. They may include multiple
#' expressions with logical operators.
#'
#' For example, to match runs with flag `batch-size` equal to 100
#' that have `loss` less than 0.8, use:
#'
#'     filter 'batch-size = 10 and loss < 0.8'
#'
#' **IMPORTANT:** You must quote EXPR if it contains spaces or
#' characters that the shell uses (e.g. '<' or '>').
#'
#' Target values may be numbers, strings or lists containing numbers
#' and strings. Strings that contain spaces must be quoted, otherwise
#' a target string values does not require quotes. Lists are defined
#' using square braces where each item is separated by a comma.
#'
#' Comparisons may use the following operators: '=', '!=' (or '<>'),
#' '<', '<=', '>', '>='. Text comparisons may use 'contains' to test
#' for case-insensitive string membership. A value may be tested for
#' membership or not in a list using 'in' or 'not in'
#' respectively. An value may be tested for undefined using 'is
#' undefined' or defined using 'is not undefined'.
#'
#' Logical operators include 'or' and 'and'. An expression may be
#' negated by preceding it with 'not'. Parentheses may be used to
#' control the order of precedence when expressions are evaluated.
#'
#' If a value reference matches more than one type of run information
#' (e.g. a flag is named 'label', which is also a run attribute), the
#' value is read in order of run attribute, then flag value, then
#' scalar. To disambiguate the reference, use a prefix `attr:`,
#' `flag:`, or `scalar:` as needed. For example, to filter using a
#' flag value named 'label', use 'flag:label'.
#'
#' Other examples:
#'
#'
#'   `operation = train and acc > 0.9`
#'   `operation = train and (acc > 0.9 or loss < 0.3)`
#'   `batch-size = 100 or batch-size = 200`
#'   `batch-size in [100,200]`
#'   `batch-size not in [400,800]`
#'   `batch-size is undefined`
#'   `batch-size is not undefined`
#'   `label contains best and operation not in [test,deploy]`
#'   `status in [error,terminated]`
#'
#' **NOTE:** Comments and tags are not supported in filter
#' expressions at this time. Use `comment` and `tag` options
#' along with filter expressions to further refine a selection.
#'
#' ### Filter by Run Start Time
#'
#' Use `started` to limit runs to those that have started within a
#' specified time range.
#'
#' **IMPORTANT:** You must quote RANGE values that contain
#' spaces. For example, to filter runs started within the last hour,
#' use the option:
#'
#'     started 'last hour'
#'
#' You can specify a time range using several different forms:
#'
#'
#'   `after DATETIME`
#'   `before DATETIME`
#'   `between DATETIME and DATETIME`
#'   `last N minutes|hours|days`
#'   `today|yesterday`
#'   `this week|month|year`
#'   `last week|month|year`
#'   `N days|weeks|months|years ago`
#'
#' `DATETIME` may be specified as a date in the format ``YY-MM-DD``
#' (the leading ``YY-`` may be omitted) or as a time in the format
#' ``HH:MM`` (24 hour clock). A date and time may be specified
#' together as `DATE TIME`.
#'
#' When using ``between DATETIME and DATETIME``, values for
#' `DATETIME` may be specified in either order.
#'
#' When specifying values like ``minutes`` and ``hours`` the trailing
#' ``s`` may be omitted to improve readability. You may also use
#' ``min`` instead of ``minutes`` and ``hr`` instead of ``hours``.
#'
#' Examples:
#'
#'
#'   `after 7-1`
#'   `after 9:00`
#'   `between 1-1 and 4-30`
#'   `between 10:00 and 15:00`
#'   `last 30 min`
#'   `last 6 hours`
#'   `today`
#'   `this week`
#'   `last month`
#'   `3 weeks ago`
#'
#' ### Filter by Source Code Digest
#'
#' To show runs for a specific source code digest, use `-g` or
#' `digest` with a complete or partial digest value.
#'
#'
#' ### Filter by Run Status
#'
#' Runs may also be filtered by specifying one or more status
#' filters: `running`, `completed`, `error`, and
#' `terminated`. These may be used together to include runs that
#' match any of the filters. For example to only include runs that
#' were either terminated or exited with an error, use ``terminated
#' error``, or the short form ``-Set``.
#'
#' You may combine more than one status character with ``-S`` to
#' expand the filter. For example, ``-Set`` shows only runs with
#' terminated or error status.
#'
#' Status filters are applied before `RUN` indexes are resolved. For
#' example, a run index of ``1`` is the latest run that matches the
#' status filters.
#'
#'
#'
#' ### Select remote rnus
#'
#' To apply `select` to remote runs, specify `remote REMOTE`. Use
#' ``guild remotes`` to list available remotes.
#'
#' For information on configuring remotes, see ``guild remotes
#' help``.
#'
#'
#' @param ... passed on to the `guild` executable. Arguments are automatically quoted with `shQuote()`, unless they are protected with `I()`. Pass `'--help'` or `help = TRUE` to see all options.
#' @param all (bool) Select all matching runs, not just the latest.
#' @param min Select the run with the lowest value for the specified COLSPEC.
#' @param max Select the run with the highest value for the specified COLSPEC.
#' @param short_id (bool) Use short ID.
#' @param attr Show specified run attribute rather than run ID.
#' @param path (bool) Show run path.
#' @param dir (bool) Show run path.
#' @param filter Filter runs using a filter expression. See Filter by Expression above for details.
#' @param operation Filter runs with operations matching `VAL`.
#' @param label Filter runs with labels matching VAL. To show unlabeled runs, use `unlabeled`.
#' @param unlabeled (bool) Filter runs without labels.
#' @param tag Filter runs with TAG.
#' @param comment Filter runs with comments matching VAL.
#' @param marked (bool) Filter marked runs.
#' @param unmarked (bool) Filter unmarked runs.
#' @param started Filter runs started within RANGE. See above for valid time ranges.
#' @param digest Filter runs with a matching source code digest.
#' @param running (bool) Filter runs that are still running.
#' @param completed (bool) Filter completed runs.
#' @param error (bool) Filter runs that exited with an error.
#' @param terminated (bool) Filter runs terminated by the user.
#' @param pending (bool) Filter pending runs.
#' @param staged (bool) Filter staged runs.
#' @param remote Run the operation remotely.
#' @keywords internal
guild_select_cli <-
function (..., all = NA, min = NULL, max = NULL, short_id = NA,
attr = NULL, path = NA, dir = NA, filter = NULL, operation = NULL,
label = NULL, unlabeled = NA, tag = NULL, comment = NULL,
marked = NA, unmarked = NA, started = NULL, digest = NULL,
running = NA, completed = NA, error = NA, terminated = NA,
pending = NA, staged = NA, remote = NULL)
{
as_cli_args(as.list.environment(environment()), ...)
}

