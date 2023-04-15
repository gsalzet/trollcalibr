#' @import methods
NULL

#' An S4 class to represent TROLL experiments.
#'
#' This is an S4 class to represent a single TROLL experiment.
#'
#' @slot id int. Experiment identifier.
#' @slot type char. Type of experiment
#' ("initialization"/ "intermediate"/ "summarize").
#' @slot func func. experiment function to apply.
#' @slot deltat int. Number of year to process after experiment.
#' @slot parameters d.f. vector of used parameters.
#' @slot inputs.opts list. List of inputs objects.
#' @slot outputs.opts list. List of outputs objects.
#'
#' @export
setClass(
  "trollexpsingle",
  representation(id = "integer",
                 type = "character",
                 func = "function",
                 deltat = "integer",
                 parameters = "data.frame",
                 inputs.opts = "list",
                 outputs.opts = "list"
  ),
  prototype(id = integer(),
            type = character(),
            func = function(x) {
              x
            },
            deltat = integer(),
            parameters = data.frame(),
            inputs.opts = list(),
            outputs.opts = list())
)

#' An S4 class to represent TROLL design and the experiments.
#'
#' This is an S4 class to represent TROLL design and the experiments.
#'
#' @param id int. Experiment identifier.
#' @param type char. Type of experiment
#' ("initialization"/ "intermediate"/ "summarize").
#' @param func func. experiment function to apply.
#' @param deltat int. Number of year to process after experiment.
#' @param parameters d.f. vector of used parameters.
#' @param inputs.opts list. List of inputs objects.
#' @param outputs.opts list. List of outputs objects.s
#'
#' @export
#' @rdname trollexpsingle
trollexpsingle <- function(id = integer(),
                     type = character(),
                     func = function(x) {
                       x
                     },
                     deltat = integer(),
                     parameters = data.frame(),
                     inputs.opts = list(),
                     outputs.opts = list()) {
  return(new("trollexpsingle",
             id = id,
             type = type,
             func = func,
             deltat = deltat,
             parameters = parameters,
             inputs.opts = inputs.opts,
             outputs.opts = outputs.opts))
}
