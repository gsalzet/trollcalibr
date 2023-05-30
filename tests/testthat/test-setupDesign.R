test_that("setupDesign", {
  
  paramsBounds <- tibble(parameter = c("klight","phi","g1","nbs0","CR_a","CR_b","Hmaxcor","habitat"),
                         quantileFn = c(\(x) {qunif(x,0.5,0.95)}, # klight
                                        \(x) {qunif(x,0.04,0.09)}, # phi
                                        \(x) {qunif(x,2,5)}, # g1
                                        \(x) {ceiling(10^qunif(x,0,3))}, # log10nbs0
                                        \(x) {qunif(x,1.5,3)}, # CR_a
                                        \(x) {qunif(x,0.4,0.8)}, # CR_b
                                        \(x) {qunif(x,0.8,1.2)},# Hmaxcor
                                        \(x) {ceiling(qunif(x,0,14))}), #Habitat 
                         type = c("global","global","global","global","global","global","species", "covariate"))
  
  ForestTest <- TROLLv3_sim@forest
  ForestInitTRUE <- rbind(ForestTest %>% mutate(simulation = "test"),ForestTest %>%
                            mutate(simulation = "test1"))
  

  
  expect_error(setupDesign(paramsBounds = matrix()), regexp = "'paramsBounds' argument of 'setupDesign' must be a data.frame")
  expect_error(setupDesign(paramsBounds = paramsBounds,ntotalsim = 10.4), regexp = "'ntotalsim' argument of 'setupDesign' must be a integer")
  expect_error(setupDesign(paramsBounds = paramsBounds,ntotalsim = 10,forestInit = ForestTest),
               regexp = "'forestInit' argument of 'setupDesign' is not formatted as detailled in 'setupDesign' help")
  
  expect_error(setupDesign(paramsBounds = paramsBounds,ntotalsim = 10,echo = FALSE,forestInit = ForestInitTRUE),
               regexp = "'dimPlot' and 'forestInit' arguments of 'setupDesign' does not share the same 'simulation' identifier")
  
  expect_s4_class(setupDesign(paramsBounds = paramsBounds,ntotalsim = 10,echo = FALSE),"trolldae")
  expect_s4_class(setupDesign(paramsBounds = paramsBounds,ntotalsim = 10,echo = FALSE,forestInit = ForestInitTRUE,
                              dimPlot = 
                                list("test" = list("xdim" = 100,"ydim" = 100),
                                     "test1" = list("xdim" = 100,"ydim" = 100))),
                  "trolldae")
})
