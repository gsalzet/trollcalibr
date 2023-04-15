#' @importFrom testthat expect_s4_class expect_type expect_true
#' @importFrom dplyr sample_frac
#' @importFrom stats setNames
NULL

#'  Process an individual experiment on a TROLL simulation.
#'
#' @param sim trollsim. a TROLL simulaiton object to process.
#' @param singleExp trollexpsingle. a trollexpsingle object to be applied on 'sim'.
#' @param parameters list.list of trollexpsingle object.
#' @param inputs list. Initial inputs for experiments.
#'
#' @return a trollexpsingle object
#' @export
#'
#' @examples
#' 
#' suppressWarnings(suppressMessages(library(dplyr)))
#' 
#' fnExpSum <- function(x,...){
#'    summary <- as.matrix(data.frame("DBH" = mean(x@forest$dbh),
#'    "LAI" = mean(x@forest$LAI)),nrow = 1,byrow = TRUE)
#'    return(summary)
#'    }
#'    
#' fnExpFrac <- function(x,parameters,...){
#'    x@forest <-  x@forest %>% sample_frac(parameters$fraction) 
#'    return(x)
#'    }  
#'    
#' Exp1 <- createExp(id = 1, 
#'  type = "Inter",
#'  deltaT = 100,
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
#'  simTest <- TROLLv3_sim
#'  
#'  processExp(sim = simTest,
#'  singleExp = Exp1,parameters = data.frame("fraction" = 0.5))
#'  
#'  processExp(sim = simTest,
#'  singleExp = Exp2)
#' 
processExp <- function(sim,
                              singleExp,
                              parameters = NULL,
                              inputs = NULL
){
  
  
  # Arguments check
  
  if (!inherits(sim, c("trollsim"))) {
    stop("'sim' argument of 'processExp' must be a trollsim")
  }
  
  if (!inherits(singleExp, c("trollexpsingle"))) {
    stop("'singleExp' argument of 'processExp' must be a trollexpsingle")
  }
  
  if (!is.null(parameters)) {
    if (!inherits(parameters, c("data.frame"))) {
      stop("'parameters' argument of 'processExp' must be a data.frame")
    }
  }
  
  if (!is.null(inputs)) {
    if (!inherits(inputs, c("list"))) {
      stop("'inputs' argument of 'processExp' must be a named list")
    }
  }
  
  
  # Global variables
 outputs <- id <- NULL
  
    if (singleExp@type != "Summary") {
      
      if (!any(!inherits(try(expect_s4_class(singleExp@func(x = sim,parameters = parameters,inputs = inputs),"trollsim"),silent = TRUE),"try-error"),
               !inherits(try(expect_s4_class(singleExp@func(x = sim,parameters = parameters,inputs = inputs)[[1]],"trollsim"),silent = TRUE),"try-error"))) {
        stop("'func' slot of 'processExp' must be provide a trollsim object or 
             a list with the first element is a trollsim")
      }
      if ((identical(singleExp@func(x = sim,parameters = parameters,inputs = inputs),sim) || 
          identical(try(singleExp@func(x = sim,parameters = parameters,inputs = inputs)[[1]],silent = TRUE),sim)) && singleExp@type != "Init") {
        warning(paste0("No change observed on tested trollsim after experiment. Check 'func' slot of singleExp ",singleExp@id))
      }
      
      if (!inherits(try(expect_s4_class(singleExp@func(x = sim,parameters = parameters,inputs = inputs),"trollsim"),silent = TRUE),"try-error")) {
        outputs <- list(singleExp@func(x = sim,parameters = parameters,inputs = inputs))
        
      }else{
        outputs <- (singleExp@func(x = sim,parameters = parameters,inputs = inputs)) 
        
        
      }
      
      if (length(outputs) == 1) {
        outputs <- setNames(outputs, c("sim"))
      }else{
        outputs <- setNames(outputs, c("sim",paste0("output_ID_",id,"_",2:length(outputs))))
      }
      
      outputs$sim@inputs$global <- update_parameters(outputs[[1]],iters = 12*singleExp@deltat)
      
    }else{
      expect_true(inherits(singleExp@func(x = sim,parameters = parameters,inputs = inputs), "matrix"))
      outputs <- list(singleExp@func(x = sim,parameters = parameters,inputs = inputs))
      
      if (length(outputs) == 1) {
        outputs <-  setNames(outputs, c("summary"))
      }else{
        outputs <-  setNames(outputs,  c("summary",paste0("output_ID_",id,"_",2:length(outputs))))
      }
    }
    
    return(trollexpsingle(id = singleExp@id,
                          type = singleExp@type,
                          func = singleExp@func,
                          deltat = singleExp@deltat,
                          parameters = if(!is.null(parameters)){parameters}else{data.frame()},
                          inputs.opts =  list(inputs),
                          outputs.opts = outputs))

 
}