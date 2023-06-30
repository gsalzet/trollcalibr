#' @include TROLLv3_sim.R
#' @importFrom parallel detectCores
#' @importFrom tidyr unnest
#' @importFrom dplyr filter_at matches vars all_vars any_vars bind_rows select
#' @importFrom parallel parLapply makeCluster clusterExport stopCluster detectCores clusterEvalQ
#' @import hetGP
#' @importFrom stats var cor cov quantile qt
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
#' @param targets list. 
#' @param nyearsStep int. Number of year to add 
#' if convergence is not reached.
#' @param nyearsMax int. Maximum number of year 
#' to reach convergence.
#' @param simOpts list for options about the surrogate modeling procedure.
#' (see \code{\link[trollcalibr:computeGP]{computeGP}} and 
#' \code{\link[trollcalibr:computeHM]{computeHM}} for details)
#' @param verbose cat processing infos.
#'
#' @return a trolldae object 
#' @export
#'
#' @examples
#' \dontrun{
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
#' nreplica = 4, sequential = TRUE,ninitsim = 50)
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
#'  }
#'
processSim <- function(dae,
                       inputs,
                       checkConverge = TRUE,
                       nyearsSampling,
                       cores,
                       filesave = NULL,
                       targets = NULL,
                       nyearsStep = 0L,
                       nyearsMax = NULL,
                       simOpts = NULL,
                       verbose = TRUE) {
  # Arguments check
  
  if (!inherits(dae, c("trolldae"))) {
    stop("'dae' argument of 'processSim' must be a trolldae")
  }
  
  if (!is.null(filesave)) {
    if (!is(filesave, c("character")) || !dir.exists(filesave)) {
      stop("'filesave' argument of 'processSim' must be a character and dir in path must exist")
    }
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
    message("'dae' state of 'processSim' is partially or totally processed for simulation step.")
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
    
    if (nyearsMax <= dae@simopts$nyearsInit) {
      stop("'nyearsMax' argument of 'processSim' must be over 'nyearsInit'")
    }
    
  }else{
    nyearsMax <- dae@simopts$nyearsInit+1
  }
  
  if(!is.null(targets)){
    if (!is(targets,"list")) {
      stop("'targets' argument of 'processSim' must be a list")
    }
    
    if (is.null(dae@experiments[[1]]@outputs.opts$summary)) {
      stop("'targets' argument of 'processSim' cannot be set for experimental setup without summary")
    }
    
    if(!all(names(targets) %in% colnames(dae@experiments[[1]]@outputs.opts$summary))){
      stop("'targets' argument of 'processSim' must include all outputs ranges")
    }
  } 
  
  if (is.null(simOpts)) {
    simOpts <- list("genPts" = dae@doeopts$nsim,
                    "ratioVal" = 0.2,
                    "nbWave" = 2,
                    "ratioProcess" = 0.1,
                    "targets" = targets,
                    "lower" = NULL,
                    "upper" = NULL,
                    "covtype" = NULL,
                    "noiseControl" = NULL,
                    "settings" = NULL)
  }
  
  IDsim <- blocki <- ID_iter <- iter <- NULL
  globalData <- speciesData <- climateData <- NULL
  dailyData <- lidarData <- forestData <- NULL
  sim_stack <- . <- nsim <- simulation <- NULL
  
  path <- dae@path
  
  dae@state <- "In-processing"
  
  setup <- dae@experiments[[1]]
  
  nyearsInit <- dae@simopts$nyearsInit
  
  nsiminit <- dae@doeopts$nsim
  
  params <- dae@params
  
  nsimtotal <- dae@doeopts$ntotalsim
  
  initk  <- dim(dae@ysim)[1]
  
  nrep <- params %>% select(IDsim) %>% 
    mutate(ID_iter = ceiling(row_number()/(cores)))
  
  dae@doeopts <- c(dae@doeopts, list("checkConverge" = if(checkConverge){data.frame("IDsim" = nrep$IDsim, nsim = NA)}else{NULL}))
  
  dae@simopts$settingsGP <- simOpts
  
  listSim <- list()
  
  summarySim <- outputsSim <- NULL
  
  if (floor(initk/(cores)) != max(nrep$ID_iter)) {
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
        
        clust <- makeCluster(cores)
        clusterExport(clust,varlist = c("globalUpdated", "params","forestInit","TROLLv3_output",
                                        "troll","select","%>%","filter","unnest","is.null"),
                      envir = environment())
        
        sim_stack <-  setNames(parLapply(clust, simulations, fun = function(x){
          sim <- troll(path = path,
                       name = x,
                       global = if(is.null(globalUpdated)){params %>% select(IDsim,globalData) %>% 
                           filter(IDsim %in% x) %>% select(-IDsim) %>%
                           unnest(cols = c(globalData))}else{globalUpdated},
                       species = params %>% select(IDsim,speciesData) %>% 
                         filter(IDsim %in% x) %>% select(-IDsim) %>%
                         unnest(cols = c(speciesData)),
                       climate = params %>% select(IDsim,climateData) %>% 
                         filter(IDsim %in% x) %>% select(-IDsim) %>% 
                         unnest(cols = c(climateData)),
                       daily = params %>% select(IDsim,dailyData) %>% 
                         filter(IDsim %in% x) %>% select(-IDsim) %>% 
                         unnest(cols = c(dailyData)),
                       verbose = FALSE,
                       lidar = if(!is.null(params$lidarData[[1]])){params %>% 
                           select(IDsim,lidarData) %>% 
                           filter(IDsim %in% x) %>% select(-IDsim) %>%
                           unnest(cols = c(lidarData))}else{NULL},
                       forest = if(!is.null(forestInit)){forestInit  %>% 
                           filter(simulation %in% x) }else{NULL},
                       overwrite = TRUE)
        if (dim(sim@forest)[1] == 0) {
          sim@forest <- TROLLv3_output@forest[1,]
          sim@forest[1,] <- NA
          
          sim@forest$iter <- max(sim@ecosystem$iter)+1
        }
        return(sim)}),simulations) %>% 
          gatherStack(name = dae@name)
        
        stopCluster(clust)
        
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
          listSimi <- splitStack(sim_stack)
          
          sim_stack_tmp <- sim_stack
        }
        
        
        
        if (checkConverge) {
          
          dae@doeopts$checkConverge$nsim[which(dae@doeopts$checkConverge$IDsim == simulations)] <- nsim
          
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
            bind_rows() %>% as.matrix()
          
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
      rm(sim_stack_tmp,listSimi,allconverged)
      
      
      if(verbose){cat(paste0("Computing TROLL Experiment # ", blocki , " / ",
                             max(nrep$ID_iter),"\n",
                             gsub(":", "-",
                                  gsub(
                                    " ", "_",
                                    date()
                                  )),"\n"))}
      
      
      
      setup_processed <- processSetup(gatherStack(listSim,dae@name), 
                                      inputs = inputs,
                                      setup = setup,
                                      saveInter = FALSE,
                                      cores = min(floor(simOpts$ratioProcess * cores),
                                                  cores))
      summarySim <- rbind(summarySim,setup_processed@outputs.opts$summary)
      outputsSim <- c(outputsSim,setup_processed@outputs.opts$outputs)
      
      listSim <- list()
      gc()
      
      if (blocki %% 10 == 0 && !is.null(filesave)) {
        save(dae,inputs, 
             checkConverge,
             nyearsSampling,
             cores,
             filesave,
             targets,
             nyearsStep,
             nyearsMax,
             simOpts, 
             summarySim,
             outputsSim,
             file = paste0(filesave,
                           "/save_iter", blocki , "-", max(nrep$ID_iter), "_", 
                           gsub(":", "-",
                                gsub(
                                  " ", "_",
                                  timestamp(
                                    prefix = "",
                                    suffix = "",
                                    quiet = T
                                  )
                                ))
                           ,".rda"))
      }
    }
    
    setup@outputs.opts$summary <- summarySim
    #setup@outputs.opts$init <- gatherStack(listSim,name = dae@name)
    setup@outputs.opts$outputs <- outputsSim
    dae@experiments <- list(setup)
    setup@outputs.opts$init <- NULL
    dae@ysim <- setup@outputs.opts$summary
    
    rm(summarySim,outputsSim,listSim)
    
    if (!is.null(filesave)) {
      save(dae,inputs, 
           checkConverge,
           nyearsSampling,
           cores,
           filesave,
           targets,
           nyearsStep,
           nyearsMax,
           simOpts, 
           file = paste0(filesave,
                         "/save_DAE_", 
                         gsub(":", "-",
                              gsub(
                                " ", "_",
                                timestamp(
                                  prefix = "",
                                  suffix = "",
                                  quiet = T
                                )
                              ))
                         ,".rda"))
    }
  }
  
  switch (dae@type[1],
          "HM" = {dae <- .historyMatching(dae = dae,
                                          simOpts = simOpts,
                                          cores = cores,
                                          checkConverge = checkConverge,
                                          filesave = filesave,
                                          nyearsSampling = nyearsSampling,
                                          nyearsStep = nyearsStep,
                                          nyearsMax = nyearsMax)},
          "GP" = {dae <- .seqDesignGP(dae = dae,
                                      simOpts = simOpts,
                                      cores = cores,
                                      checkConverge = checkConverge,
                                      filesave = filesave,
                                      nyearsSampling = nyearsSampling,
                                      nyearsStep = nyearsStep,
                                      nyearsMax = nyearsMax)}, 
          "RAW" = {dae@state <- "Post-simulation"})
  
  
  
  return(dae)
}

.fnSummary <- function(x,parameters,inputs,...){
  iter <- sum10 <- sum30 <-  NULL
  dens10 <- dens30 <- agb <- NULL
  
  data(TROLLv3_sim,envir = environment())
  
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
  
  horizons <- lapply(seq_len(dim(dae@ysim)[2]),FUN =  function(x){rep(0, nseqsim)}) 
  
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
    
    daeWIP@state <- "In-processing"
    
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
                         function(x){eval(parse(text = "update(GP@models[[x]], 
                                            Xnew=daeWIP@lhs, 
                                            Znew=daeWIP@ysim, 
                                            ginit=GP@models[[x]]$g*1.01)"))})
    
    if(seqIter %% 25 == 0){
      GP <- computeGP(dae = dae,
                      lower = simOpts$lower,
                      upper = simOpts$upper,
                      covtype = simOpts$covtype,
                      noiseControl = simOpts$noiseControl,
                      settings = simOpts$noiseControl)
    }
    
  }
  
  # dae@simopts$settingsGP <- 
    
    return(dae)
}

.historyMatching <- function(dae,
                             simOpts,
                             cores,
                             checkConverge,
                             nyearsSampling,
                             filesave,
                             nyearsStep,
                             nyearsMax){
  
  HMWIP <- computeHM(dae = dae,
                     targets = simOpts$targets,
                     genPts = floor(dae@doeopts$nsim/(1-simOpts$ratioVal)),
                     ratioVal = simOpts$ratioVal,
                     inmem = FALSE)
  
  
  
  for (wave in seq_len(simOpts$nbWav)) {
    cat(paste0("Computing TROLL history matching simulation # ", wave , " / ",
               simOpts$nbWav, "\n",
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
    
    
    
    X <- rbind(HMWIP@modelsopts$newLHS, 
               matrix(rep(t(HMWIP@modelsopts$newLHS), max(daeWIP@doeopts$nreplica -1, 0) ), 
                      ncol= dim(HMWIP@modelsopts$newLHS)[2] , byrow=TRUE))
    daeWIP@lhs <- X[sample(nrow(X)),]
    
    
    daeWIP@doeopts$nsim <- dim(daeWIP@lhs)[1]/daeWIP@doeopts$nreplica
    
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
    
    daeWIP@state <- "In-processing"
    
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
    
    HMWIP <- computeHM(dae = daeWIP,
                       targets = simOpts$targets,
                       genPts = floor(dae@doeopts$nsim/(1-simOpts$ratioVal)),
                       ratioVal = simOpts$ratioVal,
                       inmem = FALSE)
    
  }
  
  
  dae@type <- dae@type[2]
  
  return(dae)
}

