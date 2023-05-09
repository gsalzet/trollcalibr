test_that("utilsStack", {
  
  suppressWarnings(suppressMessages(library(rcontroll)))
  
  data("TROLLv3_stack",envir = environment())
  
  listSim <- splitStack(TROLLv3_stack)
  
  expect_true(all(unlist(lapply(FUN = function(x){is(x,"trollsim")},X = listSim))))
  
  expect_error(gatherStack(listSim,name = 1),
               regex = "The 'name' argument of the 'gatherStack' function must be a charcter")
  
  expect_error(gatherStack(c(listSim,1),name = "trollstack"),
               regex = "The 'listSim' argument of the 'gatherStack' function must be a list of trollsim object")
  
  stack <- gatherStack(listSim,name = "TROLLv3_stack_Ex")
  
  expect_s4_class(stack,"trollstack")
  
})