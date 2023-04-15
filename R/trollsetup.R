#' @import methods
NULL

#' An S4 class to represent spedific setup of TROLL experiments.
#'
#' This is an S4 class to represent a setup of TROLL experiments.
#'
#' @slot params d.f. vector of used parameters.
#' @slot listexp list. list of TROLL experiments.
#' @slot deltat int. cumulated simulation time.
#' @slot inputs.opts list. List of inputs objects.
#' @slot outputs.opts list. List of outputs objects.
#'
#' @export
setClass(
  "trollexpsetup",
  representation(params = "data.frame",
                 listexp = "list",
                 deltat = "integer",
                 inputs.opts = "list",
                 outputs.opts = "list"
  ),
  prototype(params = data.frame(),
            listexp = list(),
            deltat = integer(),
            inputs.opts = list(),
            outputs.opts = list())
)

#' An S4 class to represent spedific setup of TROLL experiments.
#'
#' This is an S4 class to represent a setup of TROLL experiments.
#'
#' @param params d.f. vector of used parameters.
#' @param listexp list. list of TROLL experiments.
#' @param deltat int. cumulated simulation time.
#' @param inputs.opts list. List of inputs objects.
#' @param outputs.opts list. List of outputs objects.
#'
#' @export
#' @rdname trollexpsetup
trollexpsetup <- function(params = data.frame(),
                          listexp = list(),
                          deltat = integer(),
                          inputs.opts = list(),
                          outputs.opts = list()) {
  return(new("trollexpsetup",
             params = params,
             listexp = listexp,
             deltat = deltat,
             inputs.opts = inputs.opts,
             outputs.opts = outputs.opts))
}
