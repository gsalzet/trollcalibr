#' @import methods
NULL

#' An S4 class to represent TROLL design and simulations.
#' This is an S4 class to represent TROLL design and the simulations.
#' @slot name char. Design setup name.
#' @slot lhs matrix. Latin Hypercube sample used for the design.
#' @slot parameters df. Tested parameters with design specifications.
#' @slot initopts list. Options for initial simulation.
#' @slot experiments list. list of experiments to apply.
#' @slot path char. Path to the simulation(s).
#' @slot type char. Type of design
#' (Raw : without model, GP: Gaussian processes).
#' @slot simopts list. List of  simulations options.
#'
#' @export
setClass(
  "trolldesign",
  representation(name = "character",
                 lhs = "matrix",
                 parameters = "data.frame",
                 initopts = "list",
                 experiments = "list",
                 path = "character",
                 type = "character",
                 simopts = "list"),
  prototype(name = character(),
            lhs = matrix(),
            parameters = data.frame(),
            initopts = list(),
            experiments = list(),
            path = character(),
            type = character(),
            simopts = list())
)

#' An S4 class to represent TROLL design and the simulations.
#'
#' This is an S4 class to represent TROLL design and the simulations.
#'
#' @param name char. Design setup name.
#' @param lhs matrix. Latin Hypercube sample used for the design.
#' @param parameters df. Tested parameters with design specifications.
#' @param initopts list. Options for initial simulation.
#' @param experiments list. list of experiments to apply.
#' @param path char. Path to the simulation(s).
#' @param type char. Type of design
#' (Raw : without model, GP: Gaussian processes).
#' @param simopts list. List of  simulations options.
#'
#' @export
#' @rdname trolldesign
trolldesign <- function(name = character(),
                        lhs = matrix(),
                        parameters = data.frame(),
                        initopts = list(),
                        experiments = list(),
                        path = character(),
                        type = character(),
                        simopts = list()) {
  return(new("trolldesign",
             name = name,
             lhs = lhs,
             parameters = parameters,
             initopts = initopts,
             experiments = experiments,
             path = path,
             type = type,
             simopts = simopts))
}
