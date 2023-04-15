test_that("generate_params", {
  
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
  
  fnSpecies <- \(x,species,...) {
    species_data <- species %>%
      mutate(simulation = paste0(x$IDsim)) %>%
      mutate(s_hmax = s_hmax * x$Hmaxcor) %>% 
      mutate(s_regionalfreq  = (sin(row_number() * x$habitat/14) + 1)/sum(sin(1:45 * x$habitat/14) + 1))
    return(species_data)
  }
  
  trollDAETest <- setupDesign(paramsBounds = paramsBounds,ntotalsim = 10,echo = FALSE)
  
  expect_error(generate_params(obj = trollDAETest,nyearsInit = 1.5),
               regexp = "'nyearsInit' argument of 'generate_params' must be a integer")
  
  expect_s4_class(generate_params(obj = trollDAETest,nyearsInit = 600),"trolldae")
  
  expect_s4_class(generate_params(obj = trollDAETest,nyearsInit = 600,fnSpecies = fnSpecies),
                  "trolldae")
})
