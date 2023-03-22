#' @import methods
NULL

#' An S4 class to represent TROLL experiments.
#'
#' This is an S4 class to represent TROLL experiments.
#'
#' @slot id int. Experiment identifier.
#' @slot type char. Type of experiment
#' ("initialization"/ "intermediate"/ "summarize").
#' @slot func func. experiment function to apply.
#' @slot parameters char. list of used parameters.
#' @slot inputs list. List of inputs objects.
#' @slot outputs list. List of outputs objects.
#'
#' @export
setClass(
  "trollexp",
  representation(id = "integer",
                 type = "character",
                 func = "function",
                 parameters = "character",
                 inputs = "list",
                 outputs = "list"
  ),
  prototype(id = integer(),
            type = character(),
            func = function(x) {
              x
            },
            parameters = character(),
            inputs = list(),
            outputs = list())
)

#' An S4 class to represent TROLL design and the experiments.
#'
#' This is an S4 class to represent TROLL design and the experiments.
#'
#' @param id int. Experiment identifier.
#' @param type char. Type of experiment.
#' ("initialization"/ "intermediate"/ "summarize").
#' @param func func. experiment function to appy on trollsim/ trollstack
#' @param parameters char. list of used parameters
#' @param inputs list. List of inputs objects used in experiments
#' @param outputs list. List of outputs objects used in other simulations
#'
#' @export
#' @rdname trollexp
trollexp <- function(id = integer(),
                     type = character(),
                     func = function(x) {
                       x
                     },
                     parameters = character(),
                     inputs = list(),
                     outputs = list()) {
  return(new("trollexp",
             id = id,
             type = type,
             func = func,
             parameters = parameters,
             inputs = inputs,
             outputs = outputs))
}
