#' @import methods
NULL

#' An S4 class to represent TROLL surrogate model.
#' This is an S4 class to represent TROLL surrogate model.
#' @slot type char. Type of selected surrgogate model.
#' (None/ hetGP)
#' @slot lhs matrix. Latin Hypercube sample used for the design.
#' @slot xsim matrix. unscaled tested values.
#' @slot ysim matrix. unscaled response.
#' @slot params df. Tested parameters with design specifications.
#' @slot models list. list of adjusted models.
#' @slot modelsopts list. list of model specifications.
#' @slot dae list. summary statitics on design and experiments.
#' @slot check bool. Post-hoc validation of surrogate models.
#'
#' @export
setClass(
  "surmodel",
  representation(type = "character",
                 lhs = "matrix",
                 xsim = "matrix",
                 ysim = "matrix",
                 params = "data.frame",
                 models = "list",
                 modelsopts = "list",
                 dae = "list",
                 check = "logical"),
  prototype(type = character(),
            lhs = matrix(),
            xsim = matrix(),
            ysim = matrix(),
            params = data.frame(),
            models = list(),
            modelsopts = list(),
            dae = list(),
            check = FALSE)
)

#' An S4 class to represent TROLL surrogate model.
#' This is an S4 class to represent TROLL surrogate model.
#' @param type char. Type of selected surrgogate model.
#' (None/ hetGP)
#' @param lhs matrix. Latin Hypercube sample used for the design.
#' @param xsim matrix. unscaled tested values.
#' @param ysim matrix. unscaled response.
#' @param params df. Tested parameters with design specifications.
#' @param models list. list of adjusted models.
#' @param modelsopts list. list of model specifications.
#' @param dae list. summary statitics on design and experiments.
#' @param check bool. Post-hoc validation of surrogate models.
#'
#' @export
#' @rdname surmodel
surmodel <- function(type = character(),
                     lhs = matrix(),
                     xsim = matrix(),
                     ysim = matrix(),
                     params = data.frame(),
                     models = list(),
                     modelsopts = list(),
                     dae = list(),
                     check = FALSE) {
  return(new("surmodel",
             type = type,
             lhs = lhs,
             xsim = xsim,
             ysim = ysim,
             params = params,
             models = models,
             modelsopts = modelsopts,
             dae = dae,
             check = check))
}