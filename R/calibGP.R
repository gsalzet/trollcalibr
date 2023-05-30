#' @importFrom mvtnorm dmvnorm
#' @importFrom BayesianTools createBayesianSetup createPriorDensity runMCMC getSample
#' @import hetGP
#' @importFrom stats predict terms as.formula model.matrix runif
#' @importFrom utils menu timestamp
#' @importFrom rlang f_rhs
NULL

#' A function to process calibration using a completed DAE.
#' This is an S4 class to represent TROLL surrogated calibration.
#' @param yobs d.f. Observed summary statitics.
#' @param xobs d.f. Observed variables for available summary statitics.
#' @param xcalib d.f. Available variable on the whole studied setup.
#' @param paramsFormulas list. list of equations linking estimated parameters.
#' @param covFormulas d.f. Available covariable for 'paramsFormulas' 
#' on the whole studied setup.
#' @param surmodel list. an adjusted TROLL surrogate model.
#' @param ncores int. number of core to use.
#' @param verbose bool. allow verbose
#' @param calib.opts list. Calibrations options.
#'
#' @export
#' @rdname calibGP
calibGP <- function(yobs,
                    xobs,
                    xcalib,
                    paramsFormulas,
                    covFormulas,
                    surmodel,
                    ncores,
                    verbose = TRUE,
                    calib.opts = NULL) {
  
  if (!inherits(surmodel, c("surmodel"))) {
    stop("'surmodel' argument of 'calibGP' must be a surmodel")
  }
  
  if (!surmodel@check) {
    stop("'surmodel' argument of 'calibGP' is not checked by user. Please validate surrogate model before calibration.")
  }
  
  if (length(surmodel@dae) == 0) {
    stop("'surmodel' argument of 'calibGP' must contain dae within through 'inmem' option in 'computeXX'")
  }else{
    if (!inherits(surmodel@dae[[1]], c("trolldae"))) {
      stop("'surmodel' argument of 'calibGP' must contain dae within through 'inmem' option in 'computeXX'")
    }
  }
  
  
  dae <- surmodel@dae[[1]]
  
  if (!inherits(yobs, c("data.frame"))) {
    stop("'yobs' argument of 'calibGP' must be a data.frame")
  }
  
  if (!inherits(xobs, c("data.frame"))) {
    stop("'xobs' argument of 'calibGP' must be a data.frame")
  }
  
  if (!inherits(xcalib, c("data.frame"))) {
    stop("'xcalib' argument of 'calibGP' must be a data.frame")
  }
  
  if (is.null(calib.opts)) {
    calib.opts <- list("Chains" = 4,
                       "iterations" = 10000,
                       "burnin" = 2500,
                       "packages" = NULL,
                       "variables" = NULL)
  }
  
  yF <- listGP <- out <- type <- NULL
  
  
  HM <- computeHM(dae,do.call(c,lapply(1:ncol(yF), function(x){setNames(list(c(min(yF[,x]),max(yF[,x]))), colnames(yF)[x])})),
                  ratioVal = 0,genPts = 1000)
  
  listFormulas <- .check_terms(paramsFormulas,data)
  
  nbTermsU <- do.call(rbind,lapply(1:length(listFormulas), 
                                   function(x){terms <- terms(listFormulas[[x]])
                                   return(sum(attr(terms,"order")) + sum(attr(terms,"intercept")))}))
  
  sdY <- do.call(c,lapply(1:ncol(yF), function(x){setNames(list(c(mean(yF[,x]),var(yF[,x]))), colnames(yF)[x])}))
  
  prior <- createPriorDensity(sampler = cbind(do.call(cbind,lapply(1:(dim(nbTermsU)[1]/2), function(x){
    if(nbTermsU[x,1] == 1){
      HM@modelsopts$newLHS[,x]}else{
        cbind(HM@modelsopts$newLHS[,x], rep(runif(1000,-0.5,0.5), nbTermsU[x,1]-1))}})),
    do.call(cbind, lapply(sdY,function(x){runif(1000, 0,x[2])}))),
    lower = c(do.call(c,lapply(1:(dim(nbTermsU)[1]/2), function(x){
      if(nbTermsU[x,1] == 1){
        0}else{
          c(0, rep(-1, nbTermsU[x,1]-1))}})),rep(0, sum(nbTermsU[(dim(nbTermsU)[1]/2 + 1):dim(nbTermsU)[1],1]))),
    upper = c(rep(1,sum(nbTermsU[1:(dim(nbTermsU)[1]/2),1])),
              as.numeric(do.call(c, lapply(sdY,function(x){max(2*x[2],sqrt(.Machine$double.eps))})))) )
  
  # yF <- matrix(NA, nrow = 1000,ncol = 2)
  # XFU <- as.matrix(data.frame("CR_a" = rep(0.2,1000),"CR_b" = c(rep(0.5,500),rep(0.7,500)),
  #                             "fraction" =  rbeta(1000, 2,2), "fact" = c(rep(0,500),rep(1,500))))
  # data <- data.frame("fraction" = XFU[,3], "fact" = XFU[,4])
  # X <- predict(GP@models$DBH, XFU[,1:3])
  # yF[,1] <- X$mean + rnorm(1000,0,X$sd2) + rnorm(1000,0,0.0002)
  # X <- predict(GP@models$LAI, XFU[,1:3])
  # yF[,2] <- X$mean + rnorm(1000,0,X$sd2) + rnorm(1000,0,0.002)
  # listGP <- GP@models
  # listFormulas <- .check_terms(listFormulas,data)
  # XF <- as.matrix(data[,1])
  # opts <- list(packages = list("trollcalibr"), variables = list(".lpost.invert","data" ,"XF", "yF","listGP","listFormulas" ))
  # settings <- list(iterations = 10000, nrChains= 4,  message = TRUE,burnin = 1000)
  # bayesianSetup  <- createBayesianSetup(likelihood = function(x){.lpost.invert(theta = x,XF = XF,data = data,
  #                                                                              yF = yF, listGP = listGP,
  #                                                                              listFormulas = listFormulas)},
  #                                       parallel = 2,parallelOptions = opts,
  #                                       prior = prior)
  # out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DREAMzs", settings = settings)
  # plot(out)
  # 
  
  XF <- xobs %>% select((dae@boundaries %>%  
                           filter(type %in% c("covariate", "experiment")))$parameter) %>% 
    as.matrix()
  
  opts <- list(packages = c(list("trollcalibr"),calib.opts$packages), 
               variables = c(list(".lpost.invert","data" ,"XF", "yF","listGP","paramsFormulas" ),calib.opts$variables) )
  bayesianSetup <- createBayesianSetup(likelihood = function(x){.lpost.invert(theta = x,XF = XF,data = xobs,
                                                                              yF = yF, listGP = listGP,
                                                                              listFormulas = paramsFormulas)},
                                       parallel = ncores, parallelOptions = opts,
                                       prior = prior)
  settings <- list(iterations = calib.opts$iterations, nrChains= calib.opts$Chains,  message = verbose, burnin = calib.opts$burnin)
  sampleMCMC <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DREAMzs", settings = settings)
  
  mcmc <- getSample(out,coda = TRUE,start = 1500,end = 3000, thin = 1)
  
  sample <- summary(mcmc)$quantiles[,"50%"]
  
  
  idSample <- expand.grid(1,1:ncol(yF))
  
  
  ycalib <- matrix(do.call(c,lapply(1:dim(idSample)[1],function(x){.meanGP(sample,XF = XF,data = xobs,listGP = listGP,
                                               listFormulas = paramsFormulas,yF = xobs)$p[[idSample[x,2]]]$mean})),ncol = ncol(yF))
  
  
  # x0_init <- prior$sampler(1)
  # x0_init[3] <- 0.1
  # snomadr(eval.f = .lpost.invert , XF = XF,data = xobs,
  #         yF = yF, listGP = listGP,
  #         listFormulas = paramsFormulas,bbout = 0,
  #         n = 5, bbin = rep(0,5),lb=c(0,0,0,0,0), ub=c(1,1,0.5,0.01,0.1), x0 = x0_init ,nmulti	= 10,
  #         opts = list("MAX_BB_EVAL"=1000, "INITIAL_MESH_SIZE"=0.001, "MIN_POLL_SIZE"="r0.001", "DISPLAY_DEGREE"=0))
  
  return(surcalib(yobs = yobs,
                  xobs = xobs,
                  xcalib = xcalib,
                  ycalib = data.frame(),
                  paramCalib = data.frame(),
                  paramsFormulas = list(),
                  covFormulas = data.frame(),
                  surmodel = list(),
                  calib.opts = list()))
  
}

.lpost.invert <- function(theta, XF, data, yF, listGP,listFormulas){
  nbTermsU <- do.call(rbind,lapply(1:length(listFormulas), 
                                   function(x){terms <- terms(listFormulas[[x]])
                                   return(sum(attr(terms,"order")) + sum(attr(terms,"intercept")))}))
  
  u1 <- do.call(cbind,lapply(1:length(listFormulas) ,function(x){model.matrix(as.formula(
    paste0("~ ",as.character(listFormulas[[x]])[c(3)])), data) %*% theta[if (x == 1){1:sum(nbTermsU[1,])}else{
      (sum(nbTermsU[1:(x-1),])+1):sum(nbTermsU[1:x,])} ]}))
  u <- u1[,1:(ncol(listGP[[1]]$X0) - ncol(XF))]
  s2 <- u1[,((ncol(listGP[[1]]$X0) - ncol(XF))+1):ncol(u1)]
  ## prior checking  
  if(any(u < 0 | u > 1)) {return (-Inf)}
  if(any(s2 < 0)) {return (-Inf)}
  
  ## derive predictive distribution for XF paired with u
  
  XFU <- cbind(u,XF) 
  
  p <- lapply(1:length(listGP),function(x){predict(listGP[[x]], XFU[which(!is.na(yF[,x])),], xprime = XFU[which(!is.na(yF[,x])),])})
  
  covGP <- lapply(1:length(listGP),function(x){s2[x]*diag(nrow(p[[x]]$cov)) + (p[[x]]$cov + t(p[[x]]$cov))/2})
  
  ## gaussian log density evaluation for yF under that predictive
 lln <-  sum(unlist(lapply(1:length(listGP),function(x){
   dmvnorm(x = yF[which(!is.na(yF[,x])),x], mean = p[[x]]$mean,sigma =  covGP[[x]], log=TRUE) - log(s2[x])})))
  return(lln)
}

.check_terms <- function(listFormulas,data){
  
  listFormulasDef <- NULL
  
  for (formulas in listFormulas) {
    Terms <- unlist(strsplit(labels(terms(formulas)), split = ":")) # identify parameters called to in formula
    Terms_Required <- unique(Terms) # isolate double-references (e.g. due to ":" indexing for interactions)
    Terms_Present <- Reduce(intersect, list(Terms_Required, names(data))) # identify the terms that are available and required
    if(sum(Terms_Required %in% Terms_Present) != length(Terms_Required)){
      if(length(Terms_Present) == 0){ # if none of the specified terms were found
        formulas <- paste0("Data ~ ", paste(names(data), collapse = "+"))
        warn <- paste("None of the terms specified in your formulas are present in the covariate data sets. The formulas has been altered to include all available terms in a linear model:", formulas)
      }else{ # at least some of the specified terms were found
        formulas <- paste0("Data ~ ", paste(Terms_Present, collapse = "+"))
        warn <- paste("Not all of the terms specified in your formulas are present in the covariate data sets. The formulas has been altered to include all available and specified terms in a linear model:", formulas)
      }
      Continue <- menu(c("Yes", "No"), title=paste(warn, "Do you wish to continue using the new formula?"))
      if(Continue == 2){ # break operation if user doesn't want this
        stop("Kriging terminated by user due to formula issues.")
      }
    }
    
    listFormulasDef <- c(listFormulasDef,as.formula(formulas))
  }
  
  return(listFormulasDef)
}


.meanGP <- function(theta, XF, data, listGP,listFormulas,yF){
  nbTermsU <- do.call(rbind,lapply(1:length(listFormulas), 
                                   function(x){terms <- terms(listFormulas[[x]])
                                   return(sum(attr(terms,"order")) + sum(attr(terms,"intercept")))}))
  
  u1 <- do.call(cbind,lapply(1:length(listFormulas) ,function(x){model.matrix(as.formula(
    paste0("~ ",as.character(listFormulas[[x]])[c(3)])), data) %*% theta[if (x == 1){1:sum(nbTermsU[1,])}else{
      (sum(nbTermsU[1:(x-1),])+1):sum(nbTermsU[1:x,])} ]}))
  u <- u1[,1:(ncol(listGP[[1]]$X0) - ncol(XF))]
  s2 <- u1[,((ncol(listGP[[1]]$X0) - ncol(XF))+1):ncol(u1)]
  ## prior checking  
  if(any(u < 0 | u > 1)) {return (-Inf)}
  if(any(s2 < 0)) {return (-Inf)}
  
  ## derive predictive distribution for XF paired with u
  
  XFU <- cbind(u,XF) 
  
  p <- lapply(1:length(listGP),function(x){predict(listGP[[x]], XFU[which(!is.na(yF[,x])),], xprime = XFU[which(!is.na(yF[,x])),])})
  
  covGP <- lapply(1:length(listGP),function(x){s2[x]*diag(nrow(p[[x]]$cov)) + (p[[x]]$cov + t(p[[x]]$cov))/2}) 
  
  return(list("p" = p,"covGP" = covGP,"s2" = s2))
}
