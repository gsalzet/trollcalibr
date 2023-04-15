#' @import methods
NULL

#' An S4 class to represent TROLL surrogated calibration.
#' This is an S4 class to represent TROLL surrogated calibration.
#' @slot yobs d.f. Observed summary statitics.
#' @slot xobs d.f. Observed variables for available summary statitics.
#' @slot xcalib d.f. Available variable on the whole studied setup.
#' @slot params.formulas list. list of equations linking estimated parameters.
#' @slot cov.formulas d.f. Available covariable for 'params.formulas' 
#' on the whole studied setup.
#' @slot surmodel list. an adjusted TROLL surrogate model.
#' @slot calib.opts list. Calibrations options.
#'
#' @export
setClass(
  "surcalib",
  representation(yobs = "data.frame",
                 xobs = "data.frame",
                 xcalib = "data.frame",
                 params.formulas = "list",
                 cov.formulas = "data.frame",
                 surmodel = "list",
                 calib.opts = "list"),
  prototype(yobs = data.frame(),
            xobs = data.frame(),
            xcalib = data.frame(),
            params.formulas = list(),
            cov.formulas = data.frame(),
            surmodel = list(),
            calib.opts = list())
)

#' An S4 class to represent TROLL surrogated calibration.
#' This is an S4 class to represent TROLL surrogated calibration.
#' @param yobs d.f. Observed summary statitics.
#' @param xobs d.f. Observed variables for available summary statitics.
#' @param xcalib d.f. Available variable on the whole studied setup.
#' @param params.formulas list. list of equations linking estimated parameters.
#' @param cov.formulas d.f. Available covariable for 'params.formulas' 
#' on the whole studied setup.
#' @param surmodel list. an adjusted TROLL surrogate model.
#' @param calib.opts list. Calibrations options.
#'
#' @export
#' @rdname surcalib
surcalib <- function(yobs = data.frame(),
                     xobs = data.frame(),
                     xcalib = data.frame(),
                     params.formulas = list(),
                     cov.formulas = data.frame(),
                     surmodel = list(),
                     calib.opts = list()) {
  return(new("surcalib",
             yobs = yobs,
             xobs = xobs,
             xcalib = xcalib,
             params.formulas = params.formulas,
             cov.formulas = cov.formulas,
             surmodel = surmodel,
             calib.opts = calib.opts))
}