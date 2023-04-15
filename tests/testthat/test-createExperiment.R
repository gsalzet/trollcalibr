test_that("createExp", {
  
  suppressWarnings(suppressMessages(library(dplyr)))
  fnExp1 <- function(x,...){}
  fnExp2 <- function(x,...){return(list())}
  fnExp1t <- function(x,...){x@forest <- x@forest %>% filter(dbh < 0.5)
  list(x)}
  fnExp2t <- function(x,...){
    x@forest <-  x@forest %>% sample_frac(0.5)
  return(x)
    }
  
  fnExp2s <- function(x,...){summary <- matrix(c("DBH" = mean(x@forest$dbh),"LAI" = mean(x@forest$LAI)),nrow = 1,byrow = TRUE)
  return(summary)}
  
  expect_error(createExp(id = -1,type = "Inter",deltaT = 1, fnExp = fnExp1,parameters = data.frame(),inputs = list()),
               regexp ="'id' argument of 'createExp' must be > 0" )
  
  expect_error(createExp(id = -1),
               regexp = "'fnExp' argument of 'createExp' must be a function")
  
  expect_error(createExp(id = 1,type = "Inter",deltaT = 1, fnExp = fnExp1,parameters = data.frame(),inputs = list()),
               regexp ="'fnExp' argument of 'createExp' must be provide a trollsim object or a list with the first element is a trollsim" )
  
  expect_error(createExp(id = 1,type = "Inter",deltaT = 1, fnExp = fnExp1t,parameters = matrix(),inputs = list()),
               regexp = "'parameters' argument of 'createExp' must be a data.frame")
  
  expect_s4_class(createExp(id = 0, type = "Init"),"trollexpsingle")
  
  expect_s4_class(createExp(id = 1, type = "Inter",deltaT = 100,
                                   fnExp = fnExp2t,
                                   parameters = data.frame(),inputs = list()),"trollexpsingle")
  
  expect_s4_class(createExp(id = 1, type = "Summary", fnExp = fnExp2s,parameters = data.frame(),inputs = list()),"trollexpsingle")

  expect_true(createExp(id = 1, type = "Inter",deltaT = 100,
                               fnExp = fnExp2t,
                               parameters = data.frame(),inputs = list())@outputs.opts[[1]]@inputs$global[5,2] == 100*12)
  
  expect_true(createExp(id = 1, type = "Inter",deltaT = 100,
                               fnExp = fnExp1t,
                               parameters = data.frame(),inputs = list())@outputs.opts[[1]]@inputs$global[5,2] == 100*12)
  })
