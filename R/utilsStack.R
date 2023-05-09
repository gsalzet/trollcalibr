#' @include TROLLv3_sim.R
#' @import methods
#' @importClassesFrom rcontroll trollsim trollstack
#' @importFrom tidyr nest
#' @importFrom dplyr rename bind_rows distinct
NULL

#' Function to split trollstack into a list of trollsim
#'
#' @param stack list. a trollstack to split.
#' @param ... Unused parameters.
#'
#' @return a list of trollsim
#' @export
#' 
#' @name splitStack
NULL

#' @rdname splitStack
#' @export
setGeneric("splitStack", function(stack, 
                                  ...) {
  return(standardGeneric("splitStack"))
})

#' @rdname splitStack
#' @export
setMethod("splitStack", "trollstack", function(stack,
                                                  ...){
  
  # Arguments check
  if(!inherits(stack, "trollstack")){
    stop("The 'stack' argument of the 'splitStack' function must be a trollstack object")
  }
  
  # Global variables
  simulation <- forests <- ecosystem <- species <- NULL
  lasI <- pathStack <- ysim <- listSim <- idLas <- NULL
  inputs <- IDsimi <- parameters <- forestSim <- NULL
  speciestSim <- ecosystemSim <- sim_stack <- NULL
  
  
  simulation <- stack@inputs$global$simulation %>% unique()

  forests <- stack@forest %>% nest(.by=simulation) %>% rename("forestSim" = "data") 
  ecosystem <- stack@ecosystem %>% nest(.by=simulation) %>% rename("ecosystemSim" = "data")
  species <- stack@species %>% nest(.by=simulation) %>% rename("speciestSim" = "data")
  lasI <- stack@las
  
  pathStack <- stack@path
  
  ysim <- cbind(forests,ecosystem[,2],species[,2])
  rm(forests,ecosystem,species)
  
  listSim <- list()
  
  idLas <- 0
  
  for (IDsimi in ysim$simulation) {
    idLas <- idLas + 1
    
    inputs <- list("global" = stack@inputs$global %>% 
                     filter(simulation == IDsimi) %>% select(-simulation),
                   "species" = stack@inputs$species %>% 
                     filter(simulation == IDsimi) %>% select(-simulation),
                   "climate" = stack@inputs$climate %>% 
                     filter(simulation == IDsimi) %>% select(-simulation),
                   "daily" = stack@inputs$daily %>% 
                     filter(simulation == IDsimi) %>% select(-simulation),
                   "forest" = if(dim(stack@inputs$forest)[1] !=0){
                     stack@inputs$forest %>% 
                       filter(simulation == IDsimi)}else{
                         stack@inputs$forest},
                   "lidar" = if(dim(stack@inputs$lidar)[1] !=0){
                     stack@inputs$lidar %>% 
                       filter(simulation == IDsimi)}else{
                         stack@inputs$lidar})
    
    parameters <- inputs$global$value
    names(parameters) <- inputs$global$param
    
    listSim <- c(listSim,trollsim(name = IDsimi,
                                  path = file.path(pathStack,IDsimi),
                                  parameters = parameters,
                                  forest = ysim %>% select(simulation,forestSim) %>% 
                                    filter(simulation == IDsimi) %>% select(-simulation) %>% 
                                    unnest(cols = c(forestSim)),
                                  ecosystem =
                                    ysim %>% select(simulation,ecosystemSim) %>% 
                                    filter(simulation == IDsimi) %>% select(-simulation) %>% 
                                    unnest(cols = c(ecosystemSim)),
                                  species = ysim %>% select(simulation,speciestSim) %>% 
                                    filter(simulation == IDsimi) %>% select(-simulation) %>% 
                                    unnest(cols = c(speciestSim)),
                                  las = if(length(lasI) == length(ysim$simulation)){lasI[[idLas]]}else{list()},
                                  log = stack@log[idLas],
                                  inputs = inputs))
  }
  
  listSim <- setNames(listSim,ysim$simulation) 

  return(listSim)
})

#' Function to gather trollstack into a list of trollsim
#'
#' @param listSim trollstack. 
#' @param name chr. name of the stack.
#' @param ... Unused parameters.
#'
#' @return a list of trollsim
#' @export
#'
#' 
#' @name gatherStack
NULL

#' @rdname gatherStack
#' @export
setGeneric("gatherStack", function(listSim, 
                                   name,
                                  ...) {
  return(standardGeneric("gatherStack"))
})

#' @rdname gatherStack
#' @export
setMethod("gatherStack", "list", function(listSim,
                                                name,
                                               ...){
  
  # Arguments check
  if(!all(unlist(lapply(FUN = function(x){is(x,"trollsim")},X = listSim)))){
    stop("The 'listSim' argument of the 'gatherStack' function must be a list of trollsim object")
  }
  
  if(!inherits(name, "character")){
    stop("The 'name' argument of the 'gatherStack' function must be a charcter")
  }
  
  stack_res <- slot <-  NULL
  
  stack_res <- listSim
  
  # Check inputs
  
  simulations <- lapply(listSim, slot, "name") %>% unlist()
  
  paths <- lapply(listSim, slot, "path") %>% unlist()
  
  names(stack_res) <- simulations
  
  stack_res <- trollstack(
    name = name,
    path = dirname(paths) %>% unique(),
    parameters = stack_res[[1]]@parameters,
    inputs = list(
      global = lapply(stack_res, slot, "inputs") %>%
        lapply(`[[`, "global") %>%
        bind_rows(.id = "simulation"),
      species = lapply(stack_res, slot, "inputs") %>%
        lapply(`[[`, "species") %>%
        bind_rows(.id = "simulation"),
      climate = lapply(stack_res, slot, "inputs") %>%
        lapply(`[[`, "climate") %>%
        bind_rows(.id = "simulation"),
      daily = lapply(stack_res, slot, "inputs") %>%
        lapply(`[[`, "daily") %>%
        bind_rows(.id = "simulation"),
      forest = lapply(stack_res, slot, "inputs") %>%
        lapply(`[[`, "forest") %>%
        bind_rows(.id = "simulation"),
      lidar = lapply(stack_res, slot, "inputs") %>%
        lapply(`[[`, "lidar") %>%
        bind_rows(.id = "simulation")
    ),
    log = paste(lapply(stack_res, slot, "log")),
    forest = lapply(stack_res, slot, "forest") %>%
      bind_rows(.id = "simulation"),
    ecosystem = lapply(stack_res, slot, "ecosystem") %>%
      bind_rows(.id = "simulation"),
    species = lapply(stack_res, slot, "species") %>%
      bind_rows(.id = "simulation"),
    las = lapply(stack_res, slot, "las")
  )
  
  if (nrow(stack_res@inputs$lidar) == 0) {
    stack_res@las <- list()
  }
  if (nrow(stack_res@inputs$lidar) > 0) {
    stack_res@las <- lapply(stack_res@las, `[[`, 1)
  }
  
  return(stack_res)
})
