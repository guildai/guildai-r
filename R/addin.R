
add_in_run <- function() {
  editor_context <- rstudioapi::getSourceEditorContext()
  editor_path <- editor_context$path
  if (isTRUE(nzchar(editor_path))) {
    rstudioapi::documentSaveAll()
    normalized_wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    normalized_editor_path <- normalizePath(editor_path, winslash = "/", mustWork = FALSE)
    if (grepl(paste0("^", normalized_wd), normalized_editor_path))
      editor_path <- sub(paste0("^", normalized_wd, "[/\\\\]"), "", normalized_editor_path)
    rstudioapi::sendToConsole(sprintf('guildai::guild_run("%s")', editor_path))
  } else {
    message("Unable to perform training run (active source file is not an R script)")
  }
}


add_in_view_runs <- function() {
  rstudioapi::sendToConsole("guildai::guild_view()")
}

add_in_view_run_history <- function() {
  rstudioapi::sendToConsole("View(guildai::ls_runs())")
}
