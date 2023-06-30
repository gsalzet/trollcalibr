#' @importFrom dplyr mutate row_number
NULL

#' Create a sequence of experiments to process on TROLL simulations.
#'
#' @param dae trolldae. an initialized trolldae object.
#' @param listexp list.list of trollexpsingle object.
#' @param inputs list. Initial inputs for experiments.
#'
#' @return a initialized trollexpsetup object.
#' @export
#'
#' @examples
#'   require(tibble)
#'   require(dplyr)
#' 
#' paramsBounds <- tibble(parameter = c("CR_a","CR_b","fraction"),
#'   quantileFn = c(
#'    \(x) {qunif(x,1.5,3)}, # CR_a
#'    \(x) {qunif(x,0.4,0.8)}, # CR_b
#'    \(x) {qunif(x,0.1,0.9)}# fraction
#'    ),
#'    type = c("global","global", "experiment"))
#'    
#' DAE <- setupDesign(paramsBounds = paramsBounds,ntotalsim = 20)
#'    
#' DAEwithParams <- generate_params(DAE, nyearsInit = 600)
#' 
#' fnExpSum <- function(x,...){
#'    summary <- as.matrix(data.frame("DBH" = mean(x@forest$dbh),
#'    "LAI" = mean(x@forest$LAI)))
#'    return(summary)
#'    }
#'    
#' fnExpFrac <- function(x,parameters,...){
#'    x@forest <-  x@forest %>% sample_frac(parameters$fraction) 
#'    return(x)
#'    }  
#'    
#' Exp1 <- createExp(id = 1, 
#'  type = "Inter", deltaT = 100,
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
#'  setupExperiments(dae = DAEwithParams,
#'  listexp = list(Exp1,Exp2),
#'  inputs = list("exempleInputs" = list())
#'  )
#'  
setupExperiments <- function(
    dae,
    listexp = NULL,
    inputs = NULL){
  
  # Arguments check
  
  if (!inherits(dae, "trolldae")) {
    stop("'dae' argument of 'setupExperiments' must be a trolldae")
  }
  
  if (dae@state != "Initialized") {
    stop("'state' from 'dae' argument of 'setupExperiments' must be 'Initialized'")
  }
  
  if (!is.null(listexp)) {
    if (!inherits(listexp, "list")) {
      stop("'listexp' argument of 'setupExperiments' must be a list")
    }
  }
  
  if (!is.null(inputs)) {
    if (!inherits(inputs, "list")) {
      stop("'inputs' argument of 'setupExperiments' must be a list")
    }
  }

  # Global variables
  indexesExp <- ExpI <- NULL
  finalSummaryExp <- NULL
  params <- type <- NULL
  outputs <- parameter <- NULL

  
  data(TROLLv3_sim,envir = environment())
  
  # Arguments validity check
  
  initExp <- createExp(id = 0, type = "Init",inputs = inputs)
  
  if (is.null(listexp)) {
    
    listexp <- list(initExp)
    
    return(trollexpsetup(params = data.frame(),
                         listexp = listexp,
                         deltat = as.integer(0),
                         inputs.opts = inputs,
                         outputs.opts = list("summary" = NULL,
                                             "otherOutputs" = NULL))
    )
  }else{
    
    indexExp <- 0
    
    finalSummaryExp <- FALSE
    
    finalSummary <- NULL
    Otheroutputs <- NULL
    deltaTAll <- 0
    
    for (ExpI in listexp) {
      expect_s4_class(ExpI,"trollexpsingle")
      if (ExpI@id == 0 |ExpI@type == "Init") {
        stop("'listexp' argument of 'setupExperiments' function contain not allowed
           'Init' experiment or 'id = 0' experiment in listExp")
      }
      indexExp <- indexExp +1
      if (ExpI@id != indexExp){
        stop("'listexp' argument of 'setupExperiments' function is not structured 
             with unique and sequentiallly indexed experiments")
      }
      deltaTAll <- deltaTAll + ExpI@deltat
      
      
      if (ExpI@type == "Summary" | finalSummaryExp) {
        if (!finalSummaryExp) {
          finalSummaryExp <- TRUE
        }else{
          stop("'listexp' argument of 'setupExperiments' function is not structured 
             with a unique final summary experiment")
        }
        
        
        if (length(ExpI@outputs.opts) > 1) {
          Otheroutputs <- ExpI@outputs.opts[[2:length(ExpI@outputs.opts)]]
        }
        finalSummary <- ExpI@outputs.opts$summary
      }
    }
    

  
  params <- dae@params %>% 
    select(c("IDsim",(dae@boundaries %>% 
              filter(type %in% c("experiment","covariate")) %>% 
              select(parameter))$parameter)) %>% mutate(ID = row_number())
  
  paramsTest <- params[1,]
  
  
  
  test <- TROLLv3_sim
  
  tmpExp <- NULL
  
  for (ExpI in listexp) {
    
    if (is.null(tmpExp)) {
      tmpExp <- processExp(sim = test,
                          singleExp = ExpI,
                          parameters = paramsTest,
                          inputs = inputs)
      
      outputs <- tmpExp@outputs.opts
    }else{
      
      tmpExp <- processExp(sim = outputs$sim,
                           singleExp = ExpI,
                           parameters = paramsTest,
                           inputs = append(inputs, outputs))
      outputs <- append(outputs,tmpExp@outputs.opts)
    }
    
  }
  
  
  return(trollexpsetup(params = params,
                       listexp = c(initExp,listexp),
                       deltat = as.integer(deltaTAll),
                       inputs.opts = inputs,
                       outputs.opts = list("summary" = finalSummary,
                                           "otherOutputs" = outputs))
  )
  }
}