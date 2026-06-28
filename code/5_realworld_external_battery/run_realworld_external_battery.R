# Entry point for running the taxonomy-guided real-world external battery from R.

runner_dir <- function(default = getwd()) {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]),
                                 winslash = "/", mustWork = FALSE)))
  }
  normalizePath(default, winslash = "/", mustWork = FALSE)
}

script_dir <- runner_dir()
Sys.setenv(PROJECT_ROOT = Sys.getenv("PROJECT_ROOT", unset = script_dir))

source(file.path(script_dir, "13_realworld_external_battery.R"), local = FALSE)

run_realworld_external_battery(project_root = Sys.getenv("PROJECT_ROOT", unset = script_dir))
