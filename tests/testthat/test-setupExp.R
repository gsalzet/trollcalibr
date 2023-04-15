test_that("setupExperiments", {
  
  paramsBounds <- tibble(parameter = c("CR_a","CR_b","fraction"),
                         quantileFn = c(
                           \(x) {qunif(x,1.5,3)}, # CR_a
                           \(x) {qunif(x,0.4,0.8)}, # CR_b
                           \(x) {qunif(x,0.1,0.9)}# fraction
                         ),
                         type = c("global","global", "experiment"))
  
  DAE <- setupDesign(paramsBounds = paramsBounds,ntotalsim = 20,echo = FALSE)
  
  DAEwithParams <- generate_params(DAE, nyearsInit = 600)
  
  fnExpSum <- function(x,...){
    summary <- as.matrix(data.frame("DBH" = mean(x@forest$dbh),
                                    "LAI" = mean(x@forest$LAI)))
    return(summary)
  }
  
  fnExpFrac <- function(x,parameters,...){
    x@forest <-  x@forest %>% sample_frac(parameters$fraction) 
    return(x)
  }  
  
  Exp1 <- createExp(id = 1, 
                           type = "Inter", deltaT = 100,
                           fnExp = fnExpFrac,
                           parameters = data.frame("fraction" = 0.5),
                           inputs = list())
  
  
  Exp2 <- createExp(id = 2, 
                           type = "Summary", 
                           fnExp = fnExpSum,
                           parameters = data.frame(),
                           inputs = list())
  
  Exp3 <- Exp2
  Exp3@id <- 3L
  
  
  expect_error(setupExperiments(dae = DAEwithParams,
                                   listexp = list(Exp1,Exp2,Exp3),
                                   inputs = list("exempleInputs" = list())),regexp = "listexp' argument of 'setupExperiments' function is not structured 
             with a unique final summary experiment")
  
  expect_error(setupExperiments(dae = DAEwithParams,
                                listexp = list(Exp2,Exp1),
                                inputs = list("exempleInputs" = list())),regexp = "listexp' argument of 'setupExperiments' function is not structured 
             with unique and sequentiallly indexed experiments")
  
  
  expect_s4_class(setupExperiments(dae = DAEwithParams,
                   listexp = list(Exp1,Exp2),
                   inputs = list("exempleInputs" = list())),"trollexpsetup")
})
