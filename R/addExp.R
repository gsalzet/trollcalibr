#' @include trolldae.R
#' @import methods
NULL

#' Function to add an experimental setup to a trolldae object.
#'
#' @param dae trolldae. an initialized trolldae object. 
#' @param setupExp trollexpsetup. an initialized trollexpsetup
#' @param ... Unused parameters.
#'
#' @return an initialized trolldae object
#' @export
#'
#' @examples
#' 
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
#'  addExp(DAEwithParams,setupExp12)
#' 
#' @name addExp
NULL

#' @rdname addExp
#' @export
setGeneric("addExp", function(dae,setupExp,...) {
  return(standardGeneric("addExp"))
})

#' @rdname addExp
#' @export
setMethod("addExp", "trolldae",function(dae,setupExp,...){
  
  # Arguments check
  if(!inherits(dae, "trolldae")) {
    stop("The 'dae' argument of the 'addExp' function must be a trolldae object")
  }
  
  if(length(dae@experiments) > 0) {
    stop("The 'experiments' slor of 'dae' argument of the 'addExp' function is already initialized")
  }
  
  if(!inherits(setupExp, "trollexpsetup")){
    stop("The 'setupExp' argument of the 'addExp' function must be a trollexpsetup object")
  }
  
  type <- parameter <- NULL
  
  data(TROLLv3_sim,envir = environment())
  
  test <- TROLLv3_sim
  
  tmpExp <- NULL
  
  params <- dae@params %>% 
    select(c("IDsim",(dae@boundaries %>% 
                        filter(type %in% c("experiment","covariable")) %>% 
                        select(parameter))$parameter)) %>% mutate(ID = row_number()) 
  
  paramsTest <- params[1,]
  
  inputs <- setupExp@inputs.opts
  
  listexp <- setupExp@listexp
  
  finalSummary <-NULL
  deltaTAll <- 0
  
  for (ExpI in listexp) {
    
    if (is.null(tmpExp)) {
      tmpExp <- processExp(sim = test,
                           singleExp = ExpI,
                           parameters = paramsTest,
                           inputs = inputs)
      
      outputs <- tmpExp@outputs.opts
    }else{
      
      tmpExp <-processExp(sim = outputs$sim,
                          singleExp = ExpI,
                          parameters = paramsTest,
                          inputs = append(inputs, outputs))
      outputs <- append(outputs,tmpExp@outputs.opts)
      
    }
    deltaTAll <- deltaTAll + ExpI@deltat
    
    if (ExpI@type == "Summary") {
      finalSummary <- outputs$summary
    }
    
  }
  
  TestSetup <- trollexpsetup(params = params,
                             listexp = listexp,
                             deltat = as.integer(deltaTAll),
                             inputs.opts = inputs,
                             outputs.opts = list("summary" = finalSummary,
                                                 "otherOutputs" = outputs))
  
  if(!all(identical(TestSetup@params,setupExp@params),
          identical(TestSetup@listexp,setupExp@listexp),
          identical(TestSetup@deltat,setupExp@deltat),
          identical(TestSetup@inputs.opts,setupExp@inputs.opts))){
    stop("The 'setupExp' argument of the 'addExp' function must be a trollexpsetup object")
  }
  
  dae@experiments <- list("setupExp" = setupExp)
  
  dae@state <- "Pre-process"
  
  return(dae)
})

