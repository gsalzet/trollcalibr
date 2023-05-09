#' @importFrom hmer variance_emulator_from_data generate_new_runs
NULL

#' History matching approach with hierarchical 
#' emulators (\code{\link[hmer:variance_emulator_from_data]{hmer}}) for stochastic systems
#'
#' @param dae trolldae. an initialized trolldae object.
#' @param targets  list. list of target boundaries.
#'  A pair (min, max), where min represents the lower bound and max 
#'  the upper bound for the target.
#' (see \code{hetGP::\link[hetGP:mleHetGP]{mleHetGP}} for details)
#' @param genPts int. number of points to be generated within 
#' plausible space
#' @param ratioVal num. percentage of validation points for emulator.
#' @param inmem bool. Save dae within surmodel object.
#'
#' @return a surmodel object.
#' @export
#'
#' @examples
#' 
#' 
#' 
computeHM <- function(dae,
                      targets,
                      genPts = 100,
                      ratioVal = 0.1,
                      inmem = FALSE){
  
  # Arguments check
  
  if (!is(dae,"trolldae")) {
    stop("'dae' argument of 'computeHM' must be a trolldae")
  }
  
  if (!(dae@state %in% c("In-processing","Post-simulation"))) {
    stop("'dae' argument of 'computeHM' must be a trolldae")
  }
  
  if (!is(ratioVal,"numeric")) {
    stop("'ratioVal' argument of 'computeHM' must be a numeric")
  }
  
  if (ratioVal >= 1 | ratioVal < 0) {
    stop("'ratioVal' argument of 'computeHM' must be a numeric within [0,1[")
  }
  
  if (inherits(genPts, c("numeric","integer") )) {
    if(!(as.integer(genPts) == genPts) | genPts < 0) {
      stop("'genPts' argument of 'computeHM' must be a integer >= 0")
    }
  }else{
    stop("'genPts' argument of 'computeHM' must be a integer integer >= 0")
  }
  
  ysim <- dae@ysim
  
  if (dim(ysim)[1] == 0) {
    stop("'dae' argument of 'computeHM' must have ysim processed")
  }
  
  lhs <- dae@lhs
  
  dimLhs <- dim(lhs)[2]
  dimY <- dim(ysim)[2]
  
  
  # global variables
  
  ranges <- do.call(c,lapply(seq_len(dim(dae@boundaries)[1]), function(x){
    setNames(list(c(dae@boundaries[x,]$quantileFn[[1]](0), 
                    dae@boundaries[x,]$quantileFn[[1]](1))),
             nm = dae@boundaries[x,"parameter"])}))
  
  test <- cbind(dae@xsim[,seq(dimLhs)],dae@ysim)[seq_len((1-ratioVal) * dim(ysim)[1]),]
  
  
  stoch_emulators <- variance_emulator_from_data(input_data = test, output_names = colnames(dae@ysim), ranges = ranges)
  
  LHS <- generate_new_runs(stoch_emulators,genPts , targets) %>%  as.matrix()
  
  newLHS <- do.call(cbind,setNames(lapply(seq_len(dim(LHS)[2]), 
                                          function(x){return((LHS[,x] - ranges[[x]][1])/(ranges[[x]][2] - ranges[[x]][1]))}),
                     colnames(dae@lhs)))
  
  return(surmodel(type = "HM",
                  lhs = dae@lhs,
                  xsim = dae@xsim,
                  ysim = dae@ysim,
                  params = dae@params,
                  models = list("emulators" = stoch_emulators), 
                  modelsopts = list("ratioVal" = ratioVal,
                                    "newLHS" = newLHS,
                                    "targets" = targets),
                  dae = if(inmem){list(dae)}else{list()},
                  check = FALSE))
  
}
