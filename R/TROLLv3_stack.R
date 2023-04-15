#' TROLL stack
#'
#' A TROLL outputs from a 1-year simulation on a 100x100 grid with 50 000 and 500 annual constant seed rain. 
#' Other parameters are sets todefault parameters and using TROLLv3_species, TROLLv3_climatedaytime12, and
#' TROLLv3_daytimevar for use in tests and examples. Ecosystem level output has
#' been thinned and species output has been removed to save disk space.
#'
#' @format A trollstack object.
#'
"TROLLv3_stack"

# data("TROLLv3_species")
# data("TROLLv3_climatedaytime12")
# data("TROLLv3_daytimevar")
# data("TROLLv3_output")
# TROLLv3_input_stack <- generate_parameters(
#   cols = 100, rows = 100,
#   iterperyear = 12, nbiter = 12 * 1000
# ) %>%
#   mutate(simulation = list(c("seed50000", "seed500"))) %>%
#   unnest(simulation)
# TROLLv3_input_stack[62, 2] <- 500 # Cseedrain
# stack_res <- stack(
#   name = "teststack",
#   simulations = c("seed50000", "seed500"),
#   global = TROLLv3_input_stack,
#   species = TROLLv3_species,
#   climate = TROLLv3_climatedaytime12,
#   daily = TROLLv3_daytimevar,
#   verbose = F,
#   cores = 2
# )
# usethis::use_data(TROLLv3_stack, overwrite = T)
