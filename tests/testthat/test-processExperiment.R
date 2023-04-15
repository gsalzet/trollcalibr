test_that("processExperiment", {
  suppressWarnings(suppressMessages(library(dplyr)))
  
  fnExpSum <- function(x,...){
    summary <- as.matrix(data.frame("DBH" = mean(x@forest$dbh),
                                    "LAI" = mean(x@forest$LAI)),nrow = 1,byrow = TRUE)
    return(summary)
  }
  
  fnExpFrac <- function(x,parameters,...){
    x@forest <-  x@forest %>% sample_frac(parameters$fraction) 
    return(x)
  }  
  
  Exp1 <- createExperiment(id = 1, 
                           type = "Inter",
                           deltaT = 100,
                           fnExp = fnExpFrac,
                           parameters = data.frame("fraction" = 0.5),
                           inputs = list())
  
  
  Exp2 <- createExperiment(id = 2, 
                           type = "Summary", 
                           fnExp = fnExpSum,
                           parameters = data.frame(),
                           inputs = list())
  
  simTest <- TROLLv3_sim
  

  expect_error(processExperiment(sim = Exp1,
                                 singleExp = Exp1,parameters = data.frame("fraction" = 0.5)),
               regexp = "'sim' argument of 'processExperiment' must be a trollsim")
  
  
  test1 <- processExperiment(sim = simTest,
                             singleExp = Exp1,parameters = data.frame("fraction" = 0.5))
  
  expect_s4_class(test1,
                  "trollexpsingle")
  
  expect_s4_class(test1@outputs.opts$sim,
                  "trollsim")
    
  test2 <-  processExperiment(sim = simTest,
                    singleExp = Exp2)
  
  expect_true(inherits(test2@outputs.opts$summary, "matrix"))
})
