#' @importFrom rcontroll TROLL.version
NULL

#' Options
#'
#' trollcalibr package global options.
#'
#' @param trollcalibr.tmp char. Path to temporary files folder.
#'
#' @name option.trollcalibr
NULL

.onLoad <- function(libname, pkgname) {
  tmp_dir <- file.path(tempdir(), "trollcalibr")
  dir.create(tmp_dir)
  options(list(
    trollcalibr.tmp = tmp_dir,
    trollcalibr.troll = TROLL.version()
  ))
  invisible()
}

.onUnload <- function(libpath) {
  unlink(getOption("trollcalibr.tmp"), force = TRUE, recursive = TRUE)
}
