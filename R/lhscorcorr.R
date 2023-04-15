#' @importFrom SLHD maximinSLHD
#' @importFrom Rcpp sourceCpp
NULL

#' Correlation matrix Correction
#'
#' Corrects the correlation matrix of a given Latin Hypercube Sample.
#'
#' This function changes the order in which data is organized in order
#' to force the correlation matrix to a prescribed value. This implementation
#' uses the Hungtington-Lyrintzis algorithm.
#'
#' @param vars df. mat. "raw" Latin Hypercube Sample.
#'
#' @param cormat mat. A Pearson correlation matrix.
#' @param eps num. Tolerance deviation.
#' @param echo bool. Verbose.
#' @param maxit int. maximum number of iterations.
#' (0: use a heuristic based on the size of the hypercube, -1: no stop)
#'
#' @return df. the correlation matrix corrected.
#'
#' @references
#'
#'  Chalom, A. and Prado, P.I.K.L. 2012. Parameter space exploration of ecological models
#'  \emph{arXiv}:1210.6278
#'
#' @useDynLib trollcalibr
#' 
#' @export
#'
#' @examples
#' require(SLHD)
#' uncorlhs <- maximinSLHD(t = 1,m = 100,k = 2)$StandDesign
#' corm <- matrix(c(1,0.5,0.5,1),2,2)
#' .lhscorcorr(vars = uncorlhs,cormat = corm)
#' .lhscorcorr(vars = uncorlhs,cormat = corm,maxit = 1E10)
#'
.lhscorcorr <- function(vars,
                       cormat = 0,
                       eps = 0.005,
                       echo = FALSE,
                       maxit = 0) {
  if (maxit == 0) {
    maxit <- 2 * sqrt(dim(vars)[1])
  }
    # Simply proceeds with the calculation
    return(.matcorcorr(vars, cormat, 2, eps, 1, echo, maxit))

}

.matcorcorr <- function(vars, cormat, l, eps, it, echo, maxit) {
  n <- dim(vars)[1]
  m <- dim(vars)[2]
  mynames <- colnames(vars)
  # Stop condition:
  if (l == m + 1) {
    return(vars)
  }
  # Skipping this column: correlations are close enough to the prescribed
  if (max(abs(cor(vars)[l, 1:(l - 1)] - cormat[l, 1:(l - 1)])) < eps) {
    return(.matcorcorr(vars,
                       cormat,
                       l = l + 1,
                       eps = eps,
                       it = 1,
                       echo = echo,
                       maxit = maxit))
  }
  # Skipping this column: maximum iterations for the same variable
  # If maxit is set to -1, NEVER gives up
  if (it > maxit || maxit < 0) {
    warning("lhscorcorr: correlation does 
            not converge after maximum iterations")
    return(.matcorcorr(vars,
                       cormat,
                       l = l + 1,
                       eps = eps,
                       it = 1,
                       echo = echo,
                       maxit = maxit))
  }
  if (echo == TRUE) cat(paste("Info: 
                              Correlation correction being made for l =",
                              l, "/", m, "\n"))
  # Here we start correcting the correlation for var[,l]
  vmat <- corcorr(vars = as.matrix(vars), cor = as.matrix(cormat),
                  l = as.integer(l), FLAGSTOP = as.integer(0))
  vars <- as.data.frame(matrix(vmat$vars, nrow = n, ncol = m))
  names(vars) <- mynames
  if (vmat$FLAGSTOP == 1) { # Convergence, going for next
    return(.matcorcorr(vars, cormat, l = l + 1, eps = eps,
                       it = 1, echo = echo, maxit = maxit))
  } else {
    # Repeat the proccess with the same variable
    .matcorcorr(vars, cormat, l = l, eps = eps,
                it = it + 1, echo = echo, maxit = maxit)
  }
}
