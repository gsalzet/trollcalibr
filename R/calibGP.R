#' @importFrom mvtnorm dmvnorm
#' @importFrom hetGP predict
NULL

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
#' @rdname calibGP
calibGP <- function(yobs,
                    xobs,
                    xcalib,
                    params.formulas,
                    cov.formulas,
                    surmodel,
                    calib.opts = NULL) {
  
}

.lpost.invert <- function(theta, XF, yF, GP)
{
  ## input processing and checking
  if(length(theta) != ncol(GP$X0) - ncol(XF) + 1) 
    stop("length(theta), ncol(XF), ncol(GP$X0) mismatch")
  u <- theta[-length(theta)]
  s2 <- theta[length(theta)]
  
  ## prior checking  
  if(any(u < 0 | u > 1)) return (-Inf)
  if(s2 < 0) return(-Inf)
  
  ## derive predictive distribution for XF paired with u
  XFU <- cbind(XF, matrix(rep(u, nrow(XF)), ncol=length(u), byrow=TRUE)) 
  p <- predict(GP, XFU, xprime=XFU)
  C <- s2*diag(nrow(p$cov)) + (p$cov + t(p$cov))/2 
  
  ## gaussian log density evaluation for yF under that predictive
  return(dmvnorm(yF, p$mean, C, log=TRUE) - log(s2))
}