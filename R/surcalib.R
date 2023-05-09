#' @import methods
NULL

#' An S4 class to represent TROLL surrogated calibration.
#' This is an S4 class to represent TROLL surrogated calibration.
#' @slot yobs d.f. Observed summary statitics.
#' @slot xobs d.f. Observed variables for available summary statitics.
#' @slot xcalib d.f. Available variable on the whole studied setup.
#' @slot ycalib d.f. Estimated summary statitics on the whole studied setup.
#' @slot paramCalib d.f Estimated parameters values on the whole studied setup.
#' @slot paramsFormulas list. list of equations linking estimated parameters.
#' @slot covFormulas d.f. Available covariable for 'paramCalib' 
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
                 ycalib = "data.frame",
                 paramCalib = "data.frame",
                 paramsFormulas = "list",
                 covFormulas = "data.frame",
                 surmodel = "list",
                 calib.opts = "list"),
  prototype(yobs = data.frame(),
            xobs = data.frame(),
            xcalib = data.frame(),
            ycalib = data.frame(),
            paramCalib = data.frame(),
            paramsFormulas = list(),
            covFormulas = data.frame(),
            surmodel = list(),
            calib.opts = list())
)

#' An S4 class to represent TROLL surrogated calibration.
#' This is an S4 class to represent TROLL surrogated calibration.
#' @param yobs d.f. Observed summary statitics.
#' @param xobs d.f. Observed variables for available summary statitics.
#' @param xcalib d.f. Available variable on the whole studied setup.
#' @param ycalib d.f. Estimated summary statitics on the whole studied setup.
#' @param paramCalib d.f Estimated parameters values on the whole studied setup.
#' @param paramsFormulas list. list of equations linking estimated parameters.
#' @param covFormulas d.f. Available covariable for 'paramCalib' 
#' on the whole studied setup.
#' @param surmodel list. an adjusted TROLL surrogate model.
#' @param calib.opts list. Calibrations options.
#'
#' @export
#' @rdname surcalib
surcalib <- function(yobs = data.frame(),
                     xobs = data.frame(),
                     xcalib = data.frame(),
                     ycalib = data.frame(),
                     paramCalib = data.frame(),
                     paramsFormulas = list(),
                     covFormulas = data.frame(),
                     surmodel = list(),
                     calib.opts = list()) {
  return(new("surcalib",
             yobs = yobs,
             xobs = xobs,
             xcalib = xcalib,
             ycalib = ycalib,
             paramCalib = paramCalib,
             paramsFormulas = paramsFormulas,
             covFormulas = covFormulas,
             surmodel = surmodel,
             calib.opts = calib.opts))
}