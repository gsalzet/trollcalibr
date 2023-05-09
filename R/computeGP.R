#' @importFrom hetGP find_reps allocate_mult horizon mleHetGP
NULL

#' Gaussian process modeling with heteroskedastic noise
#'
#' @param dae trolldae. an initialized trolldae object.
#' @param lower,upper  num. optional bounds for the theta parameter 
#' (see \code{hetGP::\link[hetGP:mleHetGP]{mleHetGP}} for details).
#' @param covtype covariance kernel type, either 'Gaussian', 'Matern5_2' or 'Matern3_2'.
#' (see \code{hetGP::\link[hetGP:mleHetGP]{mleHetGP}} for details)
#' @param noiseControl list with elements related to optimization of the noise process parameters.
#' (see \code{hetGP::\link[hetGP:mleHetGP]{mleHetGP}} for details)
#' @param settings list for options about the general modeling procedure
#' (see \code{hetGP::\link[hetGP:mleHetGP]{mleHetGP}} for details)
#' @param inmem bool. Save dae within surmodel object.
#'
#' @return a surmodel object.
#' @export
#'
#' @examples
#' 
#' 
#' 
computeGP <- function(dae,
                      lower = NULL,
                      upper = NULL,
                      covtype = NULL,
                      noiseControl = NULL,
                      settings = NULL,
                      inmem = FALSE){
  
  # Arguments check
  
  if (!is(dae,"trolldae")) {
    stop("'dae' argument of 'computeGP' must be a trolldae")
  }
  
  if (!is(inmem,"logical")) {
    stop("'inmem' argument of 'computeGP' must be a logical")
  }
  
  if (!(dae@state %in% c("In-processing","Post-simulation"))) {
    stop("'dae' argument of 'computeGP' must be a trolldae")
  }
  
  ysim <- dae@ysim
  
  if (dim(ysim)[1] == 0) {
    stop("'dae' argument of 'computeGP' must have ysim processed")
  }
  
  lhs <- dae@lhs
  
  dimLhs <- dim(lhs)[2]
  dimY <- dim(ysim)[2]
  
  if (!is.null(dae@simopts$settingsGP)) {
    lower <- dae@simopts$settingsGP$lower
    upper <- dae@simopts$settingsGP$upper
    covtype <- dae@simopts$settingsGP$covtype
    noiseControl <- dae@simopts$settingsGP$noiseControl
    settings <- dae@simopts$settingsGP$settings
  }
  
  if (is.null(lower)) {
    lower <- rep(0.01, dimLhs)
  }else{
    if (!is(lower,"numeric")) {
      stop("'lower' argument of 'computeGP' must be a numeric")
    }else{
      if (!(length(lower) == dimLhs || length(lower) == 1)) {
        stop("'lower' argument of 'computeGP' must have a length of 1 or # column of LHS")
      }
    }
  }
  
  if (is.null(upper)) {
    upper <- rep(30, dimLhs)
  }else{
    if (!is(upper,"numeric")) {
      stop("'upper' argument of 'computeGP' must be a numeric")
    }else{
      if (!(length(upper) == dimLhs || length(upper) == 1)) {
        stop("'upper' argument of 'computeGP' must have a length of 1 or # column of LHS")
      }
    }
  }
  
  if (is.null(covtype)) {
    covtype <- "Matern5_2"
  }else{
    if (!is(covtype,"character")) {
      stop("'covtype' argument of 'computeGP' must be a numeric")
    }else{
      if (!(covtype %in% c("Matern5_2","Matern3_2","Gaussian"))) {
        stop("'covtype' argument of 'computeGP' must match a covariance kernel type, either 'Gaussian', 'Matern5_2' or 'Matern3_2'")
      }
    }
  }

  if (is.null(noiseControl)) {
    noiseControl <- list(g_min=1e-6, g_bounds=c(1e-6, 1), lowerDelta=log(1e-6))
  }
  
  if (is.null(settings)) {
    settings <- list(linkThetas="none", initStrategy="smoothed", return.hom=TRUE)
  }
  
  # global variables
  
  prdata <- lapply(1:dimY,function(x){find_reps(lhs,ysim[,x], rescale = FALSE, normalize = FALSE)})
  
  het <- setNames(lapply(prdata, function(x){.hetGP(x,lower,upper,covtype,noiseControl,settings)}),colnames(dae@ysim))

 
  return(surmodel(type = "GP",
           lhs = dae@lhs,
           xsim = dae@xsim,
           ysim = dae@ysim,
           params = dae@params,
           models = het, 
           modelsopts = list("lower" = lower,
                             "upper" = upper,
                             "covtype" = covtype,
                             "noiseControl" = noiseControl,
                             "settings" = settings),
           dae = if(inmem){list(dae)}else{list()},
           check = FALSE))
  
}


.hetGP <- function(x,lower,upper,covtype,noiseControl,settings){
  
  mleHetGP(X = list(X0 = x$X0,Z0 = x$Z0,mult=x$mult), Z = x$Z, 
           lower=lower, upper=upper, covtype=covtype, noiseControl=noiseControl, 
                               settings=settings, maxit=1000)}
