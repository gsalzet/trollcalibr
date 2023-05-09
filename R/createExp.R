#' @importFrom testthat expect_s4_class expect_type expect_true
#' @importFrom dplyr sample_frac
#' @importFrom rcontroll update_parameters
#' @importFrom stats setNames
NULL

#'  Create individual experiment to process on TROLL simulations
#'
#' @param id int.Index of experience
#' @param type char. Type of expérience ("Init","Inter","Summary").
#' @param fnExp fct. Experiments function to be apply
#' on trollsim (see details).
#' @param deltaT int. Number of year to process after experiment.
#' @param parameters d.f. Inputs parameters from trollDAE to use within
#' fnExp function.
#' @param inputs list. Other inputs commons between experiments.
#'
#' @details
#' 'createExp' function is a formatted experiment to
#' be apply on simulation.
#' Only discrete time experiments are supported.
#' They are indexed with 'id' argument.
#' Outputs can trasmitted to the next experiments as a
#' list of output.
#' 3 types of experiments are avialables: 
#' * 'init': Initialisation of the simulation 
#' (automatically generated) ;
#' * 'inter': User defined experiments to be apply 
#' sequentially on simulations ;
#' * "Summary': User defined summary function to be 
#' apply on final trollsim.
#' 
#'
#' @return a trollexpsingle object
#' @export
#'
#' @examples
#' 
#' suppressWarnings(suppressMessages(library(dplyr)))
#' 
#' fnExpSum <- function(x,...){
#'    summary <- matrix(c("DBH" = mean(x@forest$dbh),
#'    "LAI" = mean(x@forest$LAI)),nrow = 1,byrow = TRUE)
#'    return(summary)
#'    }
#'    
#' fnExpFrac <- function(x,parameters,...){
#'    x@forest <-  x@forest %>% sample_frac(parameters$fraction) 
#'    return(list(x,matrix()))
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
createExp <- function(id,
                             type = c("Inter", "Summary", "Init"),
                             fnExp = NULL,
                             deltaT = NULL,
                             parameters = NULL,
                             inputs = NULL) {
  
  # Arguments validity check
  
  type <- match.arg(type)
  
  # Arguments check
  
  if (!inherits(id, c("integer","numeric"))) {
    stop("'id' argument of 'createExp' must be a integer")
    if (as.integer(id) == id) {
      stop("'id' argument of 'createExp' must be a integer")
    }
  }
  
  if (type != "Init" || !is.null(fnExp)) {
    if (!inherits(fnExp, c("function"))) {
      stop("'fnExp' argument of 'createExp' must be a function")
    }
  }
  
  if (type != "Init" || !is.null(parameters)) {
    if (!inherits(parameters, c("data.frame"))) {
      stop("'parameters' argument of 'createExp' must be a data.frame")
    }
  }
  
  if (type == "Inter" && is.null(deltaT)){
    stop("'deltat' argument of 'createExp' is mandatory for 'Inter' experiment")
  }
  
  if (type == "Inter" & !is.null(deltaT)) {
    if (!inherits(deltaT, c("numeric","integer"))) {
      stop("'deltaT' argument of 'createExp' must be a integer")
    }
    if (as.integer(deltaT) != deltaT) {
      stop("'deltaT' argument of 'createExp' must be a integer")
    }
    
    deltaT <- as.integer(deltaT)
  }
  
  if (type != "Init" || !is.null(inputs)) {
    if (!inherits(inputs, c("list"))) {
      stop("'inputs' argument of 'createExp' must be a list")
    }
  }
  
  
  # Global variables
  test <-  outputs <- NULL
  data(TROLLv3_sim,envir = environment())
  
  
  
  
  if(type == "Init" && id == 0){
    fnExp <- function(x,...){x}
    id <- 0
    parameters <- data.frame()

    outputs.opts <- list()
  }else{
    if (id <= 0 || (type == "Init" && id != 0)) {
      stop("'id' argument of 'createExp' must be > 0")
    }
    
    test <- TROLLv3_sim
    if (type == "Inter") {
      
      if (!any(!inherits(try(expect_s4_class(
        fnExp(x = test,parameters = parameters,inputs = inputs),
        "trollsim"),silent = TRUE),"try-error"),
               !inherits(try(expect_s4_class(
                 fnExp(x = test,parameters = parameters,inputs = inputs)[[1]],
                 "trollsim"),silent = TRUE),"try-error"))) {
        stop("'fnExp' argument of 'createExp' must be provide a trollsim object or a list with the first element is a trollsim")
      }
      if (identical(fnExp(x = test,parameters = parameters,inputs = inputs),test) || 
          identical(try(fnExp(x = test,parameters = parameters,inputs = inputs)[[1]],silent = TRUE),test)) {
        warning(paste0("No change observed on tested trollsim after experiment. 
                       Check 'fnExp' of Experiment.",id))
      }
      
      if (!inherits(try(expect_s4_class(fnExp(x = test,parameters = parameters,inputs = inputs),"trollsim"),silent = TRUE),"try-error")) {
        outputs <- list(fnExp(x = test,parameters = parameters,inputs = inputs))
        
      }else{
        outputs <- (fnExp(x = test,parameters = parameters,inputs = inputs)) 
        
        
      }
      
      if (length(outputs) == 1) {
        outputs <- setNames(outputs, c("sim"))
      }else{
        outputs <- setNames(outputs, c("sim",paste0("output_ID_",id,"_",2:length(outputs)))) 
      }
      
      outputs$sim@inputs$global <- update_parameters(outputs$sim,iters = 12*deltaT)
      
    }else{
      expect_true(inherits(fnExp(x = test,parameters = parameters,inputs = inputs), "matrix"))
      outputs <- list(fnExp(x = test,parameters = parameters,inputs = inputs))
      
      if (length(outputs) == 1) {
        outputs <- setNames(outputs, c("summary")) 
      }else{
        outputs <- setNames(outputs, c("summary",paste0("output_ID_",id,"_",2:length(outputs))))
      }
    }
    
    outputs.opts <- outputs
  }
  
  if (is.null(NULL)) {
    inputs.opts <- list()
  }else{
    inputs.opts <- inputs
  }
  
  
  return(trollexpsingle(id = as.integer(id),
                        type = type,
                        func = fnExp,
                        deltat = if(!is.null(deltaT)){deltaT}else{0L},
                        parameters = parameters,
                        inputs.opts = inputs.opts,
                        outputs.opts = outputs.opts))
}