#' @import methods
NULL

#' An S4 class to represent TROLL design and simulations.
#' This is an S4 class to represent TROLL design and the simulations.
#'
#' @slot name char. Design setup name.
#' @slot state char. Current state of design 
#'('Generated'/ 'Initialized'/ 'Pre-simulation'/ 'In-processing'/ 'Post-simulation').
#' @slot lhs matrix. Latin Hypercube sample used for the design.
#' @slot boundaries d.f. a formatted data.frame (see details) to 
#' describe parameters space.
#' @slot xsim matrix. unscaled tested values.
#' @slot ysim matrix. unscaled response (if summary experiment provided).
#' @slot params df. Tested parameters with design specifications.
#' @slot doeopts list. Options for initial simulation.
#' @slot experiments list. Setup of experiments to apply.
#' @slot path char. Path to the simulation(s).
#' @slot type char. Type of design
#' (Raw : without model, GP: Gaussian processes assisted).
#' @slot simopts list. List of  simulations options.
#' 
#' @details
#' 'boundaries' data.frame is formatted as followed:
#'  * parameter: chr. Name of the parameter within TROLL settings, 
#'  covariate table or experiment setup ;
#'  * quantileFn: fct. Quantile function to scale margin 
#'  distribution of studied parameter ;
#'  * type: chr. Type of parameter ('global'/ 'species'/ 
#'  'climate'/ 'daily'/ 'covariate/ 'experiment').
#' 
#'
#' @export
setClass(
  "trolldae",
  representation(name = "character",
                 state = "character",
                 lhs = "matrix",
                 boundaries = "data.frame",
                 xsim = "matrix",
                 ysim = "matrix",
                 params = "data.frame",
                 doeopts = "list",
                 experiments = "list",
                 path = "character",
                 type = "character",
                 simopts = "list"),
  prototype(name = character(),
            state = NA_character_,
            lhs = matrix(),
            boundaries = data.frame(),
            xsim = matrix(),
            ysim = matrix(),
            params = data.frame(),
            doeopts = list("ntotalsim" = NA_integer_,
                           "nsim" = NA_integer_,
                           "nreplica" = NA_integer_, 
                           "corrmat" = matrix(),
                           "fnParams" = list("global" = NULL,
                                             "climate" = NULL,
                                             "daily" = NULL,
                                             "lidar" = NULL),
                           "forestInit" = data.frame(),
                           "dimPlot" = list()),
            experiments = list(),
            path = character(),
            type = character(),
            simopts = list("nyearsInit" = NA_integer_, 
                           "nyearsSampling" = NA_integer_,
                           "nyearsStep" = NA_integer_,
                           "nyearsMax"= NA_integer_))
)

#' An S4 class to represent TROLL design and the simulations.
#'
#' This is an S4 class to represent TROLL design and the simulations.
#'
#' @param name char. Design setup name.
#' @param state char. char. Current state of design 
#' ('Generated'/ 'Initialized'/ 'Pre-simulation'/ 'In-processing'/ 'Post-simulation').
#' @param lhs matrix. Latin Hypercube sample used for the design.
#' @param boundaries d.f. a formatted data.frame (see details) to 
#' describe parameters space.
#' @param xsim matrix. unscaled tested values.
#' @param ysim matrix. unscaled response (if summary experiment provided).
#' @param params df. Tested params with design specifications.
#' @param doeopts list. Options for initial simulation.
#' @param experiments list. Setup of experiments to apply.
#' @param path char. Path to the simulation(s).
#' @param type char. Type of design
#' (Raw : without model, GP: Gaussian processes assisted).
#' @param simopts list. List of  simulations options.
#'
#' @details
#' 'boundaries' data.frame is formatted as followed:
#'  * parameter: chr. Name of the parameter within TROLL settings, 
#'  covariate table or experiment setup ;
#'  * quantileFn: fct. Quantile function to scale margin 
#'  distribution of studied parameter ;
#'  * type: chr. Type of parameter ('global'/ 'species'/ 
#'  'climate'/ 'daily'/ 'covariate/ 'experiment').
#'
#' @export
#' @rdname trolldae
trolldae <- function(name = character(),
                     state = NA_character_,
                     lhs = matrix(),
                     boundaries = data.frame(),
                     xsim = matrix(),
                     ysim = matrix(),
                     params = data.frame(),
                     doeopts = list("ntotalsim" = NA_integer_,
                                    "nsim" = NA_integer_,
                                    "nreplica" = NA_integer_, 
                                    "corrmat" = matrix(),
                                    "fnParams" = list("global" = NULL,
                                                      "climate" = NULL,
                                                      "daily" = NULL,
                                                      "lidar" = NULL),
                                    "forestInit" = data.frame(),
                                    "dimPlot" = list()),
                     experiments = list(),
                     path = character(),
                     type = character(),
                     simopts = list("nyearsInit" = NA_integer_, 
                                    "nyearsSampling" = NA_integer_,
                                    "nyearsStep" = NA_integer_,
                                    "nyearsMax"= NA_integer_)) {
  return(new("trolldae",
             name = name,
             state = state,
             lhs = lhs,
             boundaries = boundaries,
             xsim = xsim,
             ysim= ysim,
             params = params,
             doeopts = doeopts,
             experiments = experiments,
             path = path,
             type = type,
             simopts = simopts))
}
