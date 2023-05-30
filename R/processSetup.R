#' @import methods
#' @importClassesFrom rcontroll trollsim trollstack
#' @importFrom parallel parLapply makeCluster clusterExport stopCluster detectCores 
#' @importFrom dplyr select rename
#' @include setupExp.R
NULL

#' Function to process an a trollexpsetup on a trollsim
#'
#' @param sim trollsim.
#' @param setup trollexpsetup
#' @param inputs list.
#' @param saveInter save intermediate results
#' @param parameters d.f.
#' @param cores number of cores to use
#' @param ... Unused parameters
#'
#' @return an executed trollexpsetup.
#' 
#' @examples
#' 
#'  set.seed(123)
#'  require(tibble)
#'  require(dplyr)
#' 
#' paramsBounds <- tibble(parameter = c("CR_a","CR_b","fraction"),
#'   quantileFn = c(
#'    function(x) {qunif(x,1.5,3)}, # CR_a
#'    function(x) {qunif(x,0.4,0.8)}, # CR_b
#'    function(x) {qunif(x,0.1,0.9)}# fraction
#'    ),
#'    type = c("global","global", "experiment"))
#'    
#' DAE <- setupDesign(name = "test",paramsBounds = paramsBounds,ntotalsim = 100,  nreplica = 1,  
#' sequential = TRUE,ninitsim = 5)
#'    
#' DAEwithParams <- generate_params(DAE, nyearsInit = 1)
#' 
#' fnExpSum <- function(x,...){
#'    summary <- as.matrix(data.frame("DBH" = mean(x@forest$dbh),
#'    "LAI" = mean(x@forest$LAI)))
#'    return(summary)
#'    }
#'    
#' fnExpFrac <- function(x,parameters,...){
#'   library(dplyr)
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
#'  processSetup(TROLLv3_sim,
#'  setup = setupExp12,
#'  parameters = data.frame("fraction" = 0.5),
#'  inputs = list())
#' 
#' @name processSetup
NULL

#' @rdname processSetup
#' @export
setGeneric("processSetup", function(sim, 
                                    setup,
                                    inputs,
                                    saveInter = FALSE,
                                    parameters = NULL,
                                    cores = NULL,
                                    ...){
  return(standardGeneric("processSetup"))
}, signature = "sim")
#' @rdname processSetup
#' @export
setMethod("processSetup",c(sim = "trollsim"), function(sim,
                                                       setup,
                                                       inputs,
                                                       saveInter = FALSE,
                                                       parameters = NULL,
                                                       ...){

  # Arguments check
  
  IDsim <- NULL
  
  if (!inherits(setup, c("trollexpsetup"))) {
    stop("'setup' argument of 'processSetup' must be a trollexpsetup")
  }
  
  if (!inherits(inputs, c("list"))) {
    stop("'inputs' argument of 'processSetup' must be a list")
  }
  
  if (!inherits(saveInter, c("logical"))) {
    stop("'saveInter' argument of 'processSetup' must be a logical")
  }
    
  if (!is.null(parameters)) {
    if (!inherits(parameters, c("data.frame"))) {
      stop("'parameters' argument of 'processSetup' must be a data.frame")
    }
    if (dim(parameters)[1] > 1 ||  !all(colnames(setup@params %>% select(-c("IDsim","ID")))  %in% colnames(parameters)) ) {
      stop(paste0("'parameters' argument of 'processSetup' must be one row data.frame with column(s): ", 
                  colnames(setup@params %>% select(-"IDsim"))))
    }
  }
  
  if (is.null(parameters)) {
    if (!(sim@name %in% setup@params$IDsim)) {
      stop("'sim' argument of 'processSetup' must share a common identifier (name) with the DAE used to create 'setup' argument.
           Provide manually parameters using 'parameters' argument")
    }
    
    parameters <- setup@params %>% filter(IDsim == sim@name) 
  }

  
  
  
  #global variables
  
 
  sumSim <- interSim <-  NULL 
  outputsSim <- summarySim <- NULL 
  
  simTmp <- sim
  
  inputsTmp <- inputs
  
  for (listExp  in setup@listexp) {
    switch (listExp@type,
            "Inter" = {
              interSim <- processExp(sim = simTmp,singleExp = listExp,parameters = parameters,inputs = inputsTmp)
              simTmp <- interSim@outputs.opts$sim
              inputsTmp <- if (length(interSim@outputs.opts)>=2) {
                interSim@outputs.opts[2:length(interSim@outputs.opts)]
              }else{inputsTmp}
              
              outputsSim <- if(saveInter){
                c(outputsSim,list(interSim@outputs.opts))
                }else{
                  interSim@outputs.opts}
              
              simTmp <- troll(name = simTmp@name,
                              path = if(dir.exists(simTmp@path)){simTmp@path}else{tempdir()},
                              global = update_parameters(simTmp,nbiter = 12*listExp@deltat),
                              species = simTmp@inputs$species,
                              climate = simTmp@inputs$climate,
                              daily = simTmp@inputs$daily, 
                              lidar = if(!inherits(simTmp@inputs$lidar,"data.frame") && 
                                         !is.null(simTmp@inputs$lidar)){
                                if (dim(simTmp@inputs$lidar)[1]>0) {
                                  simTmp@inputs$lidar
                                }else{NULL}
                                }else{NULL},
                              forest = get_forest(simTmp),
                              verbose = TRUE,
                              overwrite = TRUE)
            },
            "Summary" = {
              sumSim <- processExp(sim = simTmp,singleExp = listExp,parameters = parameters,inputs = inputsTmp)
              summarySim <- sumSim@outputs.opts$summary
              outputsSim <- if(saveInter){
                if (length(sumSim@outputs.opts)>=2) {
                  c(outputsSim,list(sumSim@outputs.opts[2:length(sumSim@outputs.opts)]))
                }else{c(outputsSim,list(NULL))}
              }else{
                list(NULL)}
                
                
            })
  }
  
  if (is.null(summarySim)) {
    outputsSim <- list("summary" = list(), "outputs" = outputsSim)
  }
  
  if (saveInter) {
    setup@outputs.opts <- list("summary" = summarySim,"init" = sim,"outputs" = outputsSim)
  }else{
    setup@outputs.opts <- list("summary" = summarySim,"init" = NULL,"outputs" = outputsSim[length(outputsSim)])
  }
  setup@inputs.opts <- inputs

  setup@params <- parameters
  return(setup)
  
})

#' @rdname processSetup
#' @export
setMethod("processSetup",c(sim = "trollstack"), function(sim,
                                                         setup,
                                                         inputs,
                                                         saveInter = FALSE,
                                                         parameters = NULL,
                                                         cores = NULL,
                                                         ...){
  # Arguments check
  
  if (!inherits(setup, c("trollexpsetup"))) {
    stop("'setup' argument of 'processSetup' must be a trollexpsetup")
  }
  
  if (!is.null(parameters)) {
    if (!inherits(parameters, c("data.frame"))) {
      stop("'parameters' argument of 'processSetup' must be a data.frame")
    }
    if (!all(colnames(parameters) %in% colnames(setup@params)) ) {
      stop(paste0("'parameters' argument of 'processSetup' must be one row data.frame with column(s): ", colnames(setup@params)))
    }
  }
  
  if (is.null(parameters)) {
    if (!all(unique(sim@inputs$global$simulation) %in% setup@params$IDsim)) {
      
      if (!any(unique(sim@inputs$global$simulation) %in% setup@params$IDsim)) {
        stop("'sim' argument of 'processSetup' must share a common identifier (name) with the DAE used to create 'setup' argument.
           Provide manually parameters using 'parameters' argument")
      }else{
        warning(paste0("Not all simulation form 'sim' argument of 'processSetup' share a common identifier (name) with the DAE used to create 'setup' argument.
           \nOnly the following simulations will be proceeded: ", unique(sim@inputs$global$simulation)[unique(sim@inputs$global$simulation) %in% setup@params$IDsim],
                       "\nProvide manually parameters using 'parameters' argument"))
      }
      
    }
    
    parameters <-  setup@params[which(setup@params$IDsim %in% unique(sim@inputs$global$simulation)),] %>% 
      rename("simulation" = "IDsim")
  }
  
  if (!inherits(inputs, c("list"))) {
    stop("'inputs' argument of 'processSetup' must be a list")
  }
  
  if (!inherits(saveInter, c("logical"))) {
    stop("'saveInter' argument of 'processSetup' must be a logical")
  }
  
  if (inherits(cores, c("numeric","integer") )) {
    if(!(as.integer(cores) == cores) || cores > detectCores() - 1 || cores < 1) {
      stop("'cores' argument of 'processExp' must be a integer between 1 and detectCores() - 1")
    }
  }else{
    stop("'cores' argument of 'processExp' must be a integer between 1 and detectCores() - 1")
  }
  

  
  if (!any("simulation" %in% colnames(parameters))) {
    stop("'parameters' argument of 'processSetup' must be a data.frame with a 'simulation' column")
  }
  
  if (any(duplicated(parameters))) {
    stop("'parameters' argument of 'processSetup' must provide only unique set of parameters")
  }
  
  if (!all(parameters$simulation %in% unique(sim@inputs$global$simulation))) {
    stop("'parameters' argument of 'processSetup' must provide value for at least a subset of simulation")
  }
  
  # global variables
  
  sumstack  <- inputsTmp <- NULL
  
  allSimulations <- unique(sim@inputs$global$simulation)
  
  inputsTmp <- setNames(rep(inputs,length(allSimulations)),allSimulations)
  
  clust <- makeCluster(cores)
  clusterExport(clust,varlist = c("setup", "inputsTmp","saveInter","parameters","processSetup"),
                envir = environment())

  sumstack <- parLapply(clust,splitStack(sim),
                        fun = function(x){processSetup(sim = x,
                                                         setup = setup,
                                                         inputs = inputsTmp[which(names(inputsTmp) == x@name)],
                                                         saveInter = saveInter,
                                                         parameters = parameters[which(parameters$simulation == x@name),])})
  

  
  stopCluster(clust)
  
  setup@params <- parameters
  
  setup@inputs.opts <- inputs
  
  setup@outputs.opts <- list("summary" = do.call(rbind,lapply(sumstack,function(x){x@outputs.opts$summary})),
                             "init" = if (saveInter) {sim}else{NULL},
                             "outputs" = lapply(sumstack,function(x){x@outputs.opts$outputs}))
  
  return(setup) 
  
  
  
})