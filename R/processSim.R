#' @importFrom parallel detectCores
#' @importFrom tidyr unnest
#' @importFrom dplyr filter_at matches vars all_vars any_vars
#' @importFrom hetGP IMSPE_optim
NULL

#' A function to process simulation according to 
#' a trolldae and experimental setup.
#'
#' @param dae trolldae. an initialized trolldae object.
#' @param inputs list. Other inputs commons between experiments.
#' @param checkConverge bool. Convergence will be check.
#' To check convengence, a Gelman and Rubin’s 
#' convergence diagnostic will be used.
#' @param nyearsSampling int. Number of year to check 
#' convergence at the tail of simulation.
#' @param cores int. Number of cores to use.
#' @param filesave chr. Path to save dae in processing.
#' @param nyearsStep int. Number of year to add 
#' if convergence is not reached.
#' @param nyearsMax int. Maximum number of year 
#' to reach convergence.
#' @param simOpts list for options about the surrogate modeling procedure.
#' (see \code{hetGP::\link[hetGP:mleHetGP]{mleHetGP}} for details)
#' @param verbose cat processing infos.
#'
#' @return a trolldae object 
#' @export
#'
#' @examples
#' 
#'  set.seed(123)
#'  require(tibble)
#'  require(dplyr)
#' 
#' paramsBounds <- tibble(parameter = c("CR_a","CR_b","fraction"),
#'   quantileFn = c(
#'    \(x) {qunif(x,1.5,3)}, # CR_a
#'    \(x) {qunif(x,0.4,0.8)}, # CR_b
#'    \(x) {qunif(x,0.1,0.9)}# fraction
#'    ),
#'    type = c("global","global", "experiment"))
#'    
#' DAE <- setupDesign(name = "test",paramsBounds = paramsBounds,ntotalsim = 200,  
#' nreplica = 4, sequential = TRUE,ninitsim = 25)
#'    
#' DAEwithParams <- generate_params(DAE, nyearsInit = 2)
#' 
#' fnExpSum <- function(x,...){
#'    summary <- as.matrix(data.frame("DBH" = mean(x@forest$dbh),
#'    "LAI" = mean(x@forest$LAI)))
#'    return(summary)
#'    }
#'    
#' fnExpFrac <- function(x,parameters,...){
#'     library(dplyr)
#'    x@forest <-  x@forest %>% sample_frac(parameters$fraction) 
#'    return(x)
#'    }  
#'    
#' Exp1 <- createExp(id = 1, 
#'  type = "Inter", deltaT = 1,
#'  fnExp = fnExpFrac,
#'  parameters = data.frame("fraction" = 0.5),
#'  inputs = list())
#'   
#'    
#' Exp2 <- createExp(id = 2, 
#'  type = "Summary", 
#'  fnExp = fnExpSum,
#'  parameters = data.frame(),
#'  inputs = list())
#'  
#'  setupExp12 <- setupExperiments(dae = DAEwithParams,
#'  listexp = list(Exp1,Exp2),
#'  inputs = list("exempleInputs" = list())
#'  )
#'  
#'  DAEwithExp <- addExp(dae = DAEwithParams, setupExp = setupExp12)
#'  
#'  processSim(DAEwithExp,
#'  checkConverge = FALSE,
#'  inputs = list("exInputs" = NULL),
#'  nyearsSampling = 1,
#'  cores = 2,
#'  filesave = getwd())
#'
processSim <- function(dae,
                       inputs,
                       checkConverge = TRUE,
                       nyearsSampling,
                       cores,
                       filesave,
                       nyearsStep = 0L,
                       nyearsMax = NULL,
                       simOpts = NULL,
                       verbose = TRUE) {
  # Arguments check
  
  if (!inherits(dae, c("trolldae"))) {
    stop("'dae' argument of 'processSim' must be a trolldae")
  }
  
  if (!is(filesave, c("character")) || !dir.exists(filesave)) {
    stop("'filesave' argument of 'processSim' must be a character and dir in path must exist")
  }
  
  if (!is(checkConverge, c("logical"))) {
    stop("'checkConverge' argument of 'processSim' must be a logical")
  }
  
  if (inherits(cores, c("numeric","integer") )) {
    if(!(as.integer(cores) == cores) || cores > detectCores() - 1 || cores < 1) {
      stop("'cores' argument of 'processSim' must be a integer between 1 and detectCores() - 1")
    }
  }else{
    stop("'cores' argument of 'processSim' must be a integer between 1 and detectCores() - 1")
  }
  if (dae@state %in% c("Initialized","Generated") ) {
    stop("'dae' state of 'processSim' must be a 'Pre-process' or over.")
  }
  
  if (dae@state %in% c("In-processing","Post-simulation") ) {
    warning("'dae' state of 'processSim' is partially or totally processed for simulation step.")
  }
  
  if (!inherits(nyearsSampling, c("numeric","integer") ) || 
      !(as.integer(nyearsSampling) == nyearsSampling) && !is.null(nyearsSampling) ) {
    stop("'nyearsSampling' argument of 'processSim' must be a integer")
  }
  
  if (!is.null(nyearsMax)) {
    
    if (!inherits(nyearsStep, c("numeric","integer") ) || 
        !(as.integer(nyearsStep) == nyearsStep) && !is.null(nyearsStep)) {
      stop("'nyearsStep' argument of 'processSim' must be a integer")
    }
    
    if (!inherits(nyearsMax, c("numeric","integer","NULL") ) || 
        !(as.integer(nyearsMax) == nyearsMax) && !is.null(nyearsMax)) {
      stop("'nyearsMax' argument of 'processSim' must be a integer")
    }
    
  }else{
    nyearsMax <- dae@simopts$nyearsInit+1
  }
  
  
  if (is.null(simOpts) && dae@type == "GP") {
    simOpts <- list("lower" = NULL,
                    "upper" = NULL,
                    "covtype" = NULL,
                    "noiseControl" = NULL,
                    "settings" = NULL)
  }
  
  IDsim <- blocki <- ID_iter <- iter <- NULL
  globalData <- speciesData <- climateData <- NULL
  dailyData <- lidarData <- forestData <- NULL
  sim_stack <- NULL
  
  path <- dae@path
  
  setup <- dae@experiments[[1]]
  
  nyearsInit <- dae@simopts$nyearsInit
  
  nsiminit <- dae@doeopts$nsim
  
  params <- dae@params
  
  nsimtotal <- dae@doeopts$ntotalsim
  
  initk  <- dim(dae@ysim)[1]
  
  nrep <- params %>% select(IDsim) %>% 
    mutate(ID_iter = ceiling(row_number()/(cores)))
  
  dae@simopts$settingsGP <- simOpts
  
  listSim <- list()
  
  if (floor(initk/(cores)) +1 < max(nrep$ID_iter)) {
    
    for (blocki in ( floor(initk/(cores)) +1):max(nrep$ID_iter)) {
      
      listSimi <- list()
      
      simulations <- as.character(unlist(nrep %>% 
                                           filter(ID_iter == blocki) %>% 
                                           select(IDsim)))
      
      allconverged <- FALSE
      
      
      nyearsSim <-  nyearsInit
      
      globalUpdated <- NULL
      
      forestInit <- if(!is.null(params$forestData[[1]])){params %>% select(IDsim,forestData) %>% 
          filter(IDsim %in% simulations) %>% select(-IDsim) %>%
          unnest(cols = c(forestData))}else{NULL}
      
      sim_stack_tmp <- NULL
      
      
      while (!allconverged && nyearsSim < nyearsMax) {
        
        if(verbose){cat(paste0("Computing TROLL initial simulation # ", blocki , " / ",
                   max(nrep$ID_iter), "\n non-converged: ",length(simulations)," / ",min(cores,length(simulations))," | ",
                   gsub(":", "-",
                        gsub(
                          " ", "_",
                          date()
                        )),"\n"))}
        
        sim_stack <- stack(path = path,
                           name = "sim_stack",
                           simulations = simulations,
                           global = if(is.null(globalUpdated)){params %>% select(IDsim,globalData) %>% 
                               filter(IDsim %in% simulations) %>% select(-IDsim) %>%
                               unnest(cols = c(globalData))}else{globalUpdated},
                           species = params %>% select(IDsim,speciesData) %>% 
                             filter(IDsim %in% simulations) %>% select(-IDsim) %>%
                             unnest(cols = c(speciesData)),
                           climate = params %>% select(IDsim,climateData) %>% 
                             filter(IDsim %in% simulations) %>% select(-IDsim) %>% 
                             unnest(cols = c(climateData)),
                           daily = params %>% select(IDsim,dailyData) %>% 
                             filter(IDsim %in% simulations) %>% select(-IDsim) %>% 
                             unnest(cols = c(dailyData)),
                           verbose = FALSE,
                           lidar = if(!is.null(params$lidarData[[1]])){params %>% 
                               select(IDsim,lidarData) %>% 
                               filter(IDsim %in% simulations) %>% select(-IDsim) %>%
                               unnest(cols = c(lidarData))}else{NULL},
                           forest = forestInit,
                           cores = cores,
                           overwrite = TRUE)
        
        if (!is.null(sim_stack_tmp)) {
          sim_stack@forest <- rbind(sim_stack_tmp@forest,
                                    sim_stack@forest %>% 
                                      filter(iter >= 0 ) %>% 
                                      mutate(iter = iter + max(sim_stack_tmp@forest$iter) + 1))
          sim_stack@ecosystem <- rbind(sim_stack_tmp@ecosystem,
                                       sim_stack@ecosystem %>% 
                                         filter(iter >= 0 ) %>% 
                                         mutate(iter = iter + max(sim_stack_tmp@ecosystem$iter) + 1))
          sim_stack@species <- rbind(sim_stack_tmp@species,
                                     sim_stack@species %>% 
                                       filter(iter >= 0 ) %>% 
                                       mutate(iter = iter + max(sim_stack_tmp@species$iter) + 1))
          listSimi <- setNames(splitStack(sim_stack),simulations)
        }else{
          listSimi <- setNames(splitStack(sim_stack),simulations)
          
          sim_stack_tmp <- sim_stack
        }
        
        
        
        
        
        if (checkConverge) {
          
          expSummary <- createExp(id = 1,type = "Summary",
                                  deltaT = 1L,
                                  fnExp = .fnSummary,
                                  parameters = data.frame(),
                                  inputs = list("nyearsSim" = nyearsSim,
                                                "nyearsSampling" = nyearsSampling,
                                                "burn.in" = 0))
          
          summaryRhat <- lapply(lapply(listSimi,function(x){processExp(x,expSummary,inputs = list("nyearsSim" = nyearsSim,
                                                                                                  "nyearsSampling" = nyearsSampling,
                                                                                                  "burn.in" = 0) )}),slot,"outputs.opts") %>% 
            dplyr::bind_rows() %>% as.matrix()
          
          colnames(summaryRhat) <- colnames(expSummary@outputs.opts$summary) 
          
          summaryRhat <- summaryRhat %>% 
            as_tibble() %>% 
            mutate(simulation = names(listSimi))
          
          # print(summaryRhat)
          
          ConvergedSim <- summaryRhat %>% 
            filter_at(vars(matches("Rhat")), all_vars(.< 1.1))
          
          notConvergedSim <- summaryRhat %>% 
            filter_at(vars(matches("Rhat")), any_vars(.> 1.1))
          
          listSim <- c(listSim,listSimi[ConvergedSim$simulation])
          
          nyearsSim <- nyearsSim + nyearsStep
          
          if (dim(notConvergedSim)[1] > 0 && nyearsSim < nyearsMax) {
            
            simulations <- notConvergedSim$simulation
            
            globalUpdated <- lapply(listSimi, function(x){update_parameters(x,nbiter = 12 * nyearsStep)}) %>%
              bind_rows(.id = "simulation") %>% filter(simulation %in% simulations)
            
            forestInit <- listSimi %>%  gatherStack(name = "tmpStack") %>% get_forest() %>% filter(simulation %in% simulations)
            
            sim_stack_tmp <- listSimi[names(listSimi) %in% simulations] %>%  
              gatherStack(name = "tmpStack")
            
            
            
          }else{
            allconverged <- TRUE
            
            listSim <- c(listSim,listSimi)
          }
          
        }else{
          allconverged <- TRUE
          
          listSim <- c(listSim,listSimi)
        }
        
      }
      rm(sim_stack,sim_stack_tmp,listSimi,allconverged)
    }
    
    summarySim <- NULL
    outputsSim <- NULL
    for (blocki in seq_len(max(nrep$ID_iter))) {
      
      if(verbose){cat(paste0("Computing TROLL Experiment # ", blocki , " / ",
                             max(nrep$ID_iter),"\n",
                             gsub(":", "-",
                                  gsub(
                                    " ", "_",
                                    date()
                                  )),"\n"))}
      
      
      sim_stack <- gatherStack(listSim[which(nrep$ID_iter == blocki)],name = dae@name)
      setup_processed <- processSetup(sim_stack, 
                                      inputs = inputs,
                                      setup = setup,
                                      saveInter = FALSE,
                                      cores = cores)
      summarySim <- rbind(summarySim,setup_processed@outputs.opts$summary)
      outputsSim <- c(outputsSim,setup_processed@outputs.opts$outputs)
    }
    
    setup@outputs.opts$summary <- summarySim
    setup@outputs.opts$init <- gatherStack(listSim,name = dae@name)
    setup@outputs.opts$outputs <- outputsSim
    dae@experiments <- list(setup)
    setup@outputs.opts$init <- NULL
    dae@ysim <- setup@outputs.opts$summary
    
    rm(summarySim,outputsSim,listSim)
  }
  switch (dae@type,
          "GP" = {dae <- .seqDesignGP(dae = dae,
                                      simOpts = simOpts,
                                      cores = cores,
                                      checkConverge = checkConverge,
                                      filesave = filesave,
                                      nyearsSampling = nyearsSampling,
                                      nyearsStep = nyearsStep,
                                      nyearsMax = nyearsMax)}, 
          "RAW" = {})
  
  dae@state <- "Post-simulation"
  
  return(dae)
}

.fnSummary <- function(x,parameters,inputs,...){
  iter <- sum10 <- sum30 <-  NULL
  dens10 <- dens30 <- agb <- NULL
  
  if (identical(x,TROLLv3_sim)) {
    inputs = list("nyearsSim" = 100,
                  "nyearsSampling" = 100,
                  "burn.in" = 0)
    
    area <- (x@inputs$global$value[which(x@inputs$global$param == "cols")] * 
               x@inputs$global$value[which(x@inputs$global$param == "rows")]) * 
      x@inputs$global$value[which(x@inputs$global$param == "NV")] * 
      x@inputs$global$value[which(x@inputs$global$param == "NH")] /10000
    
    ecosystem <- x@ecosystem %>% 
      filter(iter > 12*(inputs$nyearsSim - inputs$nyearsSampling) +1) %>% 
      mutate(dens10 = sum10/area, dens30 = sum30/area ) %>% 
      select(agb, dens10, dens30) %>% as.matrix()
    
  }else{
    area <- (x@inputs$global$value[which(x@inputs$global$param == "cols")] * 
               x@inputs$global$value[which(x@inputs$global$param == "rows")]) * 
      x@inputs$global$value[which(x@inputs$global$param == "NV")] * 
      x@inputs$global$value[which(x@inputs$global$param == "NH")] /10000
    
    ecosystem <- x@ecosystem %>% 
      filter(iter > 12*(inputs$nyearsSim - inputs$nyearsSampling) +1 & iter %% 12 == 0 ) %>% 
      mutate(dens10 = sum10/area, dens30 = sum30/area ) %>% 
      select(agb, dens10, dens30) %>% as.matrix()
    
  }
  

  

    
  
  ecosystem[is.nan(ecosystem)] <- 0
  
  Rhat <- data.frame("Rhat_agb" = .Rhat(matrix(ecosystem[,1],ncol = 2),burn.in = inputs$burn.in),
                     "Rhat_dens10" = .Rhat(matrix(ecosystem[,2],ncol = 2),burn.in = inputs$burn.in),
                     "Rhat_dens30" = .Rhat(matrix(ecosystem[,3],ncol = 2),burn.in = inputs$burn.in)) %>% 
    as.matrix()
  
  return(Rhat)
}

.Rhat <-  function (M, burn.in = 0.5) {
  alpha <- .05                     # 95% intervals
  m <- ncol(M)
  x <- M [round(((burn.in * nrow(M)) + 1), 0):nrow(M),]  # second half of simulated sequences  h <- round(((burn.in * n) + 1), 0)
  n <- nrow(x)
  xdot <- as.vector(apply(x, 2, mean))
  s2 <- as.vector(apply(x, 2, var))
  W <- mean(s2)
  B <- n*var(xdot)
  muhat <- mean(xdot)
  varW <- var(s2)/m
  varB <- B^2 * 2/(m-1)
  covWB <- (n/m)*(cov(s2,xdot^2) - 2*muhat*cov(s2,xdot))
  sig2hat <- ((n-1)*W + B)/n
  quantiles <- quantile (as.vector(x), probs=c(.025,.25,.5,.75,.975))
  
  if (W > 1.e-8) {            # non-degenerate case
    postvar <- sig2hat + B/(m*n)
    varpostvar <-
      (((n-1)^2)*varW + (1+1/m)^2*varB + 2*(n-1)*(1+1/m)*covWB)/n^2
    
    chisqdf <- function(A, varA) 2 * (A^2/varA)
    
    post.df <- chisqdf (postvar, varpostvar)
    post.range <- muhat + sqrt(postvar) * qt(1-alpha/2, post.df)*c(-1,0,1)
    varlo.df <- chisqdf (W, varW)
    confshrink <- sqrt(postvar/W*(post.df+3)/(post.df+1))
    confshrink
  }
  else {      # degenerate case:  all entries in "data matrix" are identical
    1
  }
}

.seqDesignGP <- function(dae,
                         simOpts,
                         cores,
                         checkConverge,
                         nyearsSampling,
                         filesave,
                         nyearsStep,
                         nyearsMax){
  
  nseqsim <- dae@doeopts$ntotalsim - dae@doeopts$nsim
  
  horizons <- lapply(seq_len(dim(dae@ysim)[2]),FUN =  function(x){rep(0, )}) 
  
  GP <- computeGP(dae = dae,
                  lower = simOpts$lower,
                  upper = simOpts$upper,
                  covtype = simOpts$covtype,
                  noiseControl = simOpts$noiseControl,
                  settings = simOpts$noiseControl)
  
  
  for (seqIter in seq_len(nseqsim)) {
    
    cat(paste0("Computing TROLL sequential simulation # ", seqIter , " / ",
               nseqsim, "\n",
               gsub(":", "-",
                    gsub(
                      " ", "_",
                      date()
                    )),"\n"))
    
    daeWIP <- trolldae(name = dae@name,
                       boundaries = dae@boundaries,
                       doeopts = dae@doeopts,
                       state = dae@state,path = dae@path,
                       simopts = simOpts,type = "RAW")
    horizons <- lapply(seq_len(dim(dae@ysim)[2]), function(x){
      horizons[[x]][seqIter] <- horizon(GP@models[[x]])
      return(horizons[[x]])})
    
    opt <- do.call(rbind,lapply(seq_len(dim(dae@ysim)[2]), function(x){return((IMSPE_optim(GP@models[[x]],horizons[[x]][seqIter], 
                  control=list(tol_dist=1e-4, tol_diff=1e-4, multi.start=30),ncores = 1))$par)}))
    
    if (dim(opt)[1] < cores) {
      
      lhs <- rbind(opt, matrix(rep(t(opt), max(daeWIP@doeopts$nreplica -1, 0) ), ncol= dim(opt)[2] , byrow=TRUE))
      
      lhsUnique <- dae@lhs %>%  unique()
      
      repIndex <- do.call(rbind,lapply(seq_len(dim(dae@ysim)[2]), 
                           function(x){allocate_mult(GP@models[[x]],
                                                     floor((cores - dim(opt)[1])/dim(dae@ysim)[2]))})) %>% 
        colSums()
      
      X <- do.call(rbind,lapply(seq_len(dim(lhsUnique)[1]),
             function(x){matrix(rep(lhsUnique[x],
                                    repIndex[x]),
                                nrow = repIndex[x], 
                                ncol = dim(lhsUnique)[2],
                                byrow = TRUE)}))
      
      X <- rbind(rbind(X, matrix(rep(t(X), max(daeWIP@doeopts$nreplica -1, 0) ), ncol= dim(lhs)[2] , byrow=TRUE)),lhs)
      
      X <- X[sample(nrow(X)),]
      
      daeWIP@lhs <- X
      
      daeWIP@doeopts$nsim <- dim(daeWIP@lhs)[1]/daeWIP@doeopts$nreplica
      
    }else{
      lhs <- rbind(opt, matrix(rep(t(opt), max(daeWIP@doeopts$nreplica -1, 0) ), ncol= dim(opt)[2] , byrow=TRUE))
      
      lhs <- lhs[sample(nrow(lhs)),]
      
      colnames(lhs) <- colnames(dae@lhs)
      daeWIP@lhs <- lhs
      
      daeWIP@doeopts$nsim <- dim(daeWIP@lhs)[1]/daeWIP@doeopts$nreplica

      
    }
    
     
    
    
    daeWIP <- generate_params(obj = daeWIP,
                    nyearsInit = dae@simopts$nyearsInit,
                    fnGlobal = dae@doeopts$fnParams$fnGlobal,
                    fnClimate = dae@doeopts$fnParams$fnClimate,
                    climateInit = dae@doeopts$fnParams$climateInit,
                    fnDaily = dae@doeopts$fnParams$fnDaily,
                    dailyInit = dae@doeopts$fnParams$dailyInit,
                    fnSpecies = dae@doeopts$fnParams$fnSpecies,
                    speciesInit = dae@doeopts$fnParams$speciesInit,
                    fnLidar = dae@doeopts$fnParams$fnLidar,
                    covariates = dae@doeopts$fnParams$covariates,
                    echo = FALSE, minID = dim(dae@ysim)[1])

    setup <- setupExperiments(dae = daeWIP,
                              listexp = dae@experiments[[1]]@listexp[2:length(dae@experiments[[1]]@listexp)],
                              inputs = dae@experiments[[1]]@inputs.opts)
    
    daeWIP <- addExp(daeWIP,setup)
    
    daeWIP <- processSim(dae = daeWIP,
                         inputs = dae@experiments[[1]]@inputs.opts,
                         checkConverge = checkConverge,cores = cores,
                         nyearsSampling = nyearsSampling,filesave = filesave,
                         nyearsStep = nyearsStep, nyearsMax = nyearsMax, simOpts = simOpts,
                         verbose = FALSE)
    
    dae@lhs <- rbind(dae@lhs, daeWIP@lhs)
    
    dae@params <- rbind(dae@params, daeWIP@params)
    
    dae@doeopts$nsim <- dae@doeopts$nsim + daeWIP@doeopts$nsim
    
    dae@xsim <- rbind(dae@xsim,daeWIP@xsim)
    dae@ysim <- rbind(dae@ysim,daeWIP@ysim)
    
    GP@models <-  lapply(seq_len(dim(dae@ysim)[2]),
                         function(x){update(GP@models[[x]], 
                                            Xnew=daeWIP@lhs, 
                                            Znew=daeWIP@ysim, 
                                            ginit=GP@models[[x]]$g*1.01)})
    
    if(seqIter %% 25 == 0){
      GP <- computeGP(dae = dae,
                      lower = simOpts$lower,
                      upper = simOpts$upper,
                      covtype = simOpts$covtype,
                      noiseControl = simOpts$noiseControl,
                      settings = simOpts$noiseControl)
    }
    
  }
  
  return(dae)
}

