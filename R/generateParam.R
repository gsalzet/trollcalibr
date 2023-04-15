#' @include trolldae.R
#' @import methods
#' @import rcontroll
#' @importFrom dplyr mutate row_number select filter if_else rowwise do
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang is_empty
NULL

#' Function to generate settings parameters for TROLL simulator 
#' from a trollDAE object.
#'
#' @param obj trolldae. 
#' @param nyearsInit int. Minimal number of years for initialization.
#' @param fnGlobal fct. Custom function to compute global TROLL settings.
#' if not provided, quantiles functions in boundaries will be used.
#' @param fnClimate fct. Custom function to compute climate TROLL settings.
#' if not provided, quantiles functions in boundaries will be used to modify *mean value*.
#' @param climateInit d.f. Reference climate data to use in simulation.
#' @param fnDaily fct. Custom function to compute daily variations TROLL settings.
#' if not provided, quantiles functions in boundaries will be used to modify *amplitude by multiplying* values.
#' @param dailyInit d.f. Reference daily variations data to use in simulation.
#' @param fnSpecies fct. Custom function to compute species TROLL settings.
#' if not provided, quantiles functions in boundaries will be used to modify *mean value*.
#' @param speciesInit d.f.Reference species data to use in simulation.
#' @param fnLidar fct. Custom function to compute lidar TROLL settings.
#' if not provided, *no lidar* simulation performed.
#' @param covariates d.f. Covariates used in custom functions to 
#' compute TROLL settings.
#' @param echo bool. display information messages (default = TRUE).
#' @param minID int. Minimal simulation index.
#' @param ... Unused parameters.
#'
#' @return an initialized trolldae object
#' @export
#'
#' @examples
#' require(tibble)
#' require(dplyr)
#' paramsBounds <- tibble(parameter = c("klight","phi","g1",
#' "nbs0","CR_a","CR_b","Hmaxcor","habitat"),
#'   quantileFn = c(\(x) {qunif(x,0.5,0.95)}, # klight
#'    \(x) {qunif(x,0.04,0.09)}, # phi
#'    \(x) {qunif(x,2,5)}, # g1
#'    \(x) {ceiling(10^qunif(x,0,3))}, # log10nbs0
#'    \(x) {qunif(x,1.5,3)}, # CR_a
#'    \(x) {qunif(x,0.4,0.8)}, # CR_b
#'    \(x) {qunif(x,0.8,1.2)},# Hmaxcor
#'    \(x) {ceiling(qunif(x,0,14))}), #Habitat 
#'    type = c("global","global","global","global",
#'    "global","global","species", "covariate"))
#'    
#' fnSpecies <- \(x,species,...) {
#'    species_data <- species %>%
#'    mutate(simulation = paste0(x$IDsim)) %>%
#'    mutate(s_hmax = s_hmax * x$Hmaxcor) %>% 
#'    mutate(s_regionalfreq  = (sin(row_number() * 
#'    x$habitat/14) + 1)/sum(sin(1:45 * x$habitat/14) + 1))
#'    return(species_data)
#'    }
#' 
#' trollDAETest <- setupDesign(paramsBounds = paramsBounds,
#' ntotalsim = 10,echo = FALSE)
#' 
#' generate_params(obj = trollDAETest,nyearsInit = 600)
#' 
#' @name generate_params
NULL

#' @rdname generate_params
#' @export
setGeneric("generate_params", function(obj,
                                       nyearsInit,
                                       fnGlobal = NULL,
                                       fnClimate = NULL,
                                       climateInit = NULL,
                                       fnDaily = NULL,
                                       dailyInit = NULL,
                                       fnSpecies = NULL,
                                       speciesInit = NULL,
                                       fnLidar = NULL,
                                       covariates = NULL,
                                       echo = FALSE, 
                                       minID = NULL,
                                       ...) {
  return(standardGeneric("generate_params"))
})

#' @rdname generate_params
#' @export
setMethod("generate_params", "trolldae", function(obj,
                                                  nyearsInit,
                                                  fnGlobal = NULL,
                                                  fnClimate = NULL,
                                                  climateInit = NULL,
                                                  fnDaily = NULL,
                                                  dailyInit = NULL,
                                                  fnSpecies = NULL,
                                                  speciesInit = NULL,
                                                  fnLidar = NULL,
                                                  covariates = NULL,
                                                  echo = FALSE,
                                                  minID = 0L,
                                                  ...){
  
  # Arguments check
  if(!inherits(obj, "trolldae")){
    stop("The 'obj' argument of the 'generate_params' function must be a trolldae object")
  }
  
  if(!(dim(obj@lhs)[1] == (obj@doeopts$nsim * obj@doeopts$nreplica * length(obj@doeopts$dimPlot))  &&
       dim(obj@lhs)[2] == (dim(obj@boundaries)[1] + as.integer(!is.null(obj@doeopts$forestInit))))) {
    stop("The 'lhs' and 'boundaries slots from the 'obj' object of the 'generate_params' function have incorrect dimensions.\n
         Re-initialize a valide 'obj' trolldae object with 'setupDesign' function.")
  }
  
  if (!inherits(nyearsInit, c("numeric","integer") ) | 
      !(as.integer(nyearsInit) == nyearsInit) & !is.null(nyearsInit)) {
    stop("'nyearsInit' argument of 'generate_params' must be a integer")
  }
  
  if (!inherits(minID, c("numeric","integer") ) | 
      !(as.integer(minID) == minID) & !is.null(minID)) {
    stop("'minID' argument of 'generate_params' must be a integer")
  }
  
  
  
  # Global variables
  . <- name <- X <- Xsim <- NULL
  params <- globalData <- speciesData <- NULL
  climateData <- dailyData <- lidarData <- NULL
  j  <- param <- parameter <- NULL
  paramsBounds <- type <- NULL
  
  name <- obj@name
  
  X <- obj@lhs
  
  paramsBounds <- obj@boundaries
  
  Xsim <- as_tibble(X)
  
  init <- 0
  
  if (!is.null(obj@doeopts$forestInit)) {
    init <- 1
  }
  
  for (j in 1:(dim(obj@boundaries)[1])) {
    Xsim[,init + j] <- paramsBounds$quantileFn[[j]](X[, init + j])
  }
  
  default_global <- generate_parameters(nbiter = 1,iterperyear = 12) %>% 
    select(param) %>% unlist() %>% as.character()
  default_climate <- colnames(rcontroll::TROLLv3_climatedaytime12)
  default_daily <- colnames(rcontroll::TROLLv3_daytimevar)
  default_species <- colnames(rcontroll::TROLLv3_species)
  default_lidar <- generate_lidar(iter_pointcloud_generation = 12 * nyearsInit) %>% 
    select(param) %>% unlist() %>% as.character()
  
  globalParam <- paramsBounds %>% 
    filter(type == "global" & parameter %in% c(default_global,"fallocleaf")|
             type %in% c("global","covariate") & !is.null(fnGlobal))
  if(is.null(fnGlobal)){fnGlobal <- .fnGlobal}
  
  climateParam <- paramsBounds %>% 
    filter(type == "climate" & parameter %in% c(default_climate)|
             type %in% c("climate","covariate") & !is.null(fnClimate))
  if(is.null(fnClimate)){fnClimate <- .fnClimate}
  if(is.null(climateInit)){climateInit <- rcontroll::TROLLv3_climatedaytime12}
  
  dailyParam <- paramsBounds %>% 
    filter(type == "daily" & parameter %in% c(default_daily)|
             type %in% c("daily","covariate") & !is.null(fnDaily))
  if(is.null(fnDaily)){fnDaily <- .fnDaily}
  if(is.null(dailyInit)){dailyInit <- rcontroll::TROLLv3_daytimevar}
  
  speciesParam <- paramsBounds %>% 
    filter(type == "species" & parameter %in% c(default_species)|
             type %in% c("species","covariate") & !is.null(fnSpecies))
  if(is.null(fnSpecies)){fnSpecies <- .fnSpecies}
  if(is.null(speciesInit)){speciesInit <- rcontroll::TROLLv3_species}
  
  lidarParam <- paramsBounds %>% 
    filter(type == "lidar" & parameter %in% c(default_lidar)|
             type %in% c("lidar","covariate") & !is.null(fnLidar))
  if(is.null(fnLidar)){fnLidar <- .fnLidar}
  
  if (!is.null(minID)) {
    params <- Xsim %>% 
      mutate(IDsim = paste0(name,"_",minID + row_number())) %>% 
      rowwise()
  }else{
    params <- Xsim %>% 
      mutate(IDsim = paste0(name,"_",row_number())) %>% 
      rowwise()
  }
   
  
  if (echo) {
    message("Computing global parameters")
  }
  globalData <- params %>% select(if(is.null(obj@doeopts$forestInit)){c("IDsim",globalParam$parameter)}else{
    c("IDsim","ForestID",globalParam$parameter)}) %>% 
    do(globalData = fnGlobal(., dimPlot = obj@doeopts$dimPlot,nyears = nyearsInit,covariates))
  if (echo) {
    message("Computing species parameters")
  }
  speciesData <- params %>% select(if(is.null(obj@doeopts$forestInit)){c("IDsim",speciesParam$parameter)}else{
    c("IDsim","ForestID",speciesParam$parameter)}) %>% 
    do(speciesData = fnSpecies(.,species = speciesInit, covariates))
  if (echo) {
    message("Computing climate parameters")
  }
  climateData <- params %>% select(c("IDsim",climateParam$parameter)) %>% 
    do(climateData = fnClimate(.,climate = climateInit,covariates))
  if (echo) {
    message("Computing daily parameters")
  }
  dailyData <- params %>% select(c("IDsim",dailyParam$parameter)) %>% 
    do(dailyData = fnDaily(., daily = dailyInit, covariates))
  if (echo) {
    message("Computing lidar parameters")
  }
  lidarData <- params %>% select(c("IDsim",lidarParam$parameter)) %>% 
    do(lidarData = fnLidar(.,nyears = nyearsInit,covariates))
  
  if (echo) {
    message("Computing forest parameters")
  }
  if(!is.null(obj@doeopts$forestInit)){
    
    forestInit <-obj@doeopts$forestInit
    forestData <- params %>% select(c("IDsim","ForestID")) %>%
      do(forestData = .fnForest(.,forestInit))
  }else{
    .fnForest <- function(x){NULL}
    forestData <- params %>% select(c("IDsim")) %>%
      do(forestData = .fnForest(.))
  }
  

  
  
  
  obj@xsim <- as.matrix(Xsim)
  
  obj@params <- cbind(params,globalData,speciesData,climateData,dailyData,lidarData,forestData)
  
  obj@doeopts <- list("ntotalsim" = obj@doeopts$ntotalsim,
                      "nsim" = obj@doeopts$nsim,
                      "nreplica" = obj@doeopts$nreplica, 
                      "corrmat" = obj@doeopts$corrmat,
                      "fnParams" = list("fnGlobal" = fnGlobal,
                                        "fnClimate" = fnClimate,
                                        "climateInit" = climateInit,
                                        "fnSpecies" = fnSpecies,
                                        "speciesInit" = speciesInit,
                                        "fnDaily" = fnDaily,
                                        "dailyInit" = dailyInit,
                                        "fnLidar" = fnLidar,
                                        "covariates" = covariates),
                      "forestInit" = obj@doeopts$forestInit,
                      "dimPlot" = obj@doeopts$dimPlot)
  
  obj@state <- "Initialized"
  
  obj@simopts$nyearsInit <- as.integer(nyearsInit)
  
  return(obj)
})

# Define default global 

.fnGlobal <- function(x,dimPlot, nyears, ...){
  
  defaultGlobalData <- generate_parameters(nbiter = 0,iterperyear = 12)
  globalData <- generate_parameters(cols = as.numeric(if(!suppressWarnings(is_empty(x$ForestID))){dimPlot[[x$ForestID]]$ydim}else{dimPlot[[1]]$ydim}),
                                    rows = as.numeric(if(!suppressWarnings(is_empty(x$ForestID))){dimPlot[[x$ForestID]]$xdim}else{dimPlot[[1]]$xdim}),
                                    iterperyear = 12,
                                    nbiter = nyears*12,
                                    klight = if(!suppressWarnings(is_empty(x$klight))){x$klight}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "klight"),"value"]},
                                    phi = if(!suppressWarnings(is_empty(x$phi))){x$phi}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "phi"),"value"]},
                                    g1 = if(!suppressWarnings(is_empty(x$g1))){x$g1}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "g1"),"value"]},
                                    fallocwood = if(!suppressWarnings(is_empty(x$fallocwood))){x$fallocwood}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "fallocwood"),"value"]},
                                    falloccanopy = if(!suppressWarnings(is_empty(x$falloccanopy))){x$falloccanopy}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "falloccanopy"),"value"]},
                                    m = if(!suppressWarnings(is_empty(x$m))){x$m}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "m"),"value"]},
                                    vC = if(!suppressWarnings(is_empty(x$vC))){x$vC}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "vC"),"value"]},
                                    Cseedrain = if(!suppressWarnings(is_empty(x$Cseedrain))){x$Cseedrain}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "Cseedrain"),"value"]},
                                    nbs0 = if(!suppressWarnings(is_empty(x$nbs0))){x$nbs0}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "nbs0"),"value"]},
                                    CR_a = if(!suppressWarnings(is_empty(x$CR_a))){x$CR_a}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "CR_a"),"value"]},
                                    CR_b = if(!suppressWarnings(is_empty(x$CR_b))){x$CR_b}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "CR_b"),"value"]}, 
                                    m1 = if(!suppressWarnings(is_empty(x$m1))){x$m1}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "m1"),"value"]},
                                    NONRANDOM = 2,
                                    sigma_height = if(!suppressWarnings(is_empty(x$sigma_height))){x$sigma_height}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "sigma_height"),"value"]},
                                    sigma_CR = if(!suppressWarnings(is_empty(x$sigma_CR))){x$sigma_CR}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "sigma_CR"),"value"]},
                                    sigma_P = if(!suppressWarnings(is_empty(x$sigma_P))){x$sigma_P}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "sigma_P"),"value"]},
                                    sigma_N = if(!suppressWarnings(is_empty(x$sigma_N))){x$sigma_N}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "sigma_N"),"value"]},
                                    sigma_LMA = if(!suppressWarnings(is_empty(x$sigma_LMA))){x$sigma_LMA}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "sigma_LMA"),"value"]},
                                    sigma_wsg = if(!suppressWarnings(is_empty(x$sigma_wsg))){x$sigma_wsg}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "sigma_wsg"),"value"]},
                                    corr_CR_height = if(!suppressWarnings(is_empty(x$corr_CR_height))){x$corr_CR_height}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "corr_CR_height"),"value"]},
                                    corr_N_LMA = if(!suppressWarnings(is_empty(x$corr_N_LMA))){x$corr_N_LMA}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "corr_N_LMA"),"value"]},
                                    corr_N_P = if(!suppressWarnings(is_empty(x$corr_N_P))){x$corr_N_P}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "corr_N_P"),"value"]},
                                    corr_P_LMA = if(!suppressWarnings(is_empty(x$corr_P_LMA))){x$corr_P_LMA}else{
                                      defaultGlobalData[which(defaultGlobalData$param == "corr_P_LMA"),"value"]},
                                    OUTPUT_extended = 1)  %>% 
    mutate(simulation = paste0(x$IDsim))
  
  return(globalData)
}

.fnLidar <- function(x, nyear, ...){
  NULL
}

.fnClimate <- function(x, climate, ...){
  dataClimate <- climate %>% 
    mutate(simulation = paste0(x$IDsim))
  
  dataClimate$Temperature <- if (!suppressWarnings(is_empty(x$Temperature))) {
    x$Temperature + dataClimate$Temperature - mean(dataClimate$Temperature)
  }else{dataClimate$Temperature}
  
  dataClimate$DaytimeMeanTemperature <- if (!suppressWarnings(is_empty(x$DaytimeMeanTemperature))) {
    x$DaytimeMeanTemperature + dataClimate$DaytimeMeanTemperature - mean(dataClimate$DaytimeMeanTemperature)
  }else{dataClimate$DaytimeMeanTemperature}
  
  dataClimate$NightTemperature <- if (!suppressWarnings(is_empty(x$NightTemperature))) {
    x$NightTemperature + dataClimate$NightTemperature - mean(dataClimate$NightTemperature)
  }else{dataClimate$NightTemperature}
  
  dataClimate$Rainfall <- if (!suppressWarnings(is_empty(x$Rainfall))) {
    x$Rainfall + dataClimate$Rainfall - mean(dataClimate$Rainfall)
  }else{dataClimate$Rainfall}
  
  dataClimate$WindSpeed <- if (!suppressWarnings(is_empty(x$WindSpeed))) {
    x$WindSpeed + dataClimate$WindSpeed - mean(dataClimate$WindSpeed)
  }else{dataClimate$WindSpeed}
  
  dataClimate$DaytimeMeanIrradiance <- if (!suppressWarnings(is_empty(x$DaytimeMeanIrradiance))) {
    x$DaytimeMeanIrradiance + dataClimate$DaytimeMeanIrradiance - mean(dataClimate$DaytimeMeanIrradiance)
  }else{dataClimate$DaytimeMeanIrradiance}
  
  dataClimate$MeanIrradiance <- if (!suppressWarnings(is_empty(x$MeanIrradiance))) {
    x$MeanIrradiance + dataClimate$MeanIrradiance - mean(dataClimate$MeanIrradiance)
  }else{dataClimate$MeanIrradiance}
  
  return(dataClimate)
}

.fnDaily <- function(x,daily, ...){
  dataDaily <- daily %>% 
    mutate(simulation = paste0(x$IDsim))
  
  dataDaily$vardaytime_light <- if (!suppressWarnings(is_empty(x$vardaytime_light))) {
    x$vardaytime_light * dataDaily$vardaytime_light
  }else{dataDaily$vardaytime_light}
  
  dataDaily$vardaytime_vpd <- if (!suppressWarnings(is_empty(x$vardaytime_vpd))) {
    x$vardaytime_vpd * dataDaily$vardaytime_vpd 
  }else{dataDaily$vardaytime_vpd}
  
  dataDaily$vardaytime_T <- if (!suppressWarnings(is_empty(x$vardaytime_T))) {
    x$vardaytime_T * dataDaily$vardaytime_T
  }else{dataDaily$vardaytime_T}
  
  return(dataDaily)
}

.fnSpecies <- function(x,species, ...){
  dataSpecies <- species %>% 
    mutate(simulation = paste0(x$IDsim))
  
  dataSpecies$s_LMA <- if (!suppressWarnings(is_empty(x$s_LMA))) {
    x$s_LMA + dataSpecies$s_LMA - mean(dataSpecies$s_LMA)
  }else{dataSpecies$s_LMA}
  
  dataSpecies$s_Nmass <- if (!suppressWarnings(is_empty(x$s_Nmass))) {
    x$s_Nmass + dataSpecies$s_Nmass - mean(dataSpecies$s_Nmass)
  }else{dataSpecies$s_Nmass}
  
  dataSpecies$s_Pmass <- if (!suppressWarnings(is_empty(x$s_Pmass))) {
    x$s_Pmass + dataSpecies$s_Pmass - mean(dataSpecies$s_Pmass)
  }else{dataSpecies$s_Pmass}
  
  dataSpecies$s_wsg <- if (!suppressWarnings(is_empty(x$s_wsg))) {
    x$s_wsg + dataSpecies$s_wsg - mean(dataSpecies$s_wsg)
  }else{dataSpecies$s_wsg}
  
  dataSpecies$s_dbhmax <- if (!suppressWarnings(is_empty(x$s_dbhmax))) {
    x$s_dbhmax + dataSpecies$s_dbhmax - mean(dataSpecies$s_dbhmax)
  }else{dataSpecies$s_dbhmax}
  
  dataSpecies$s_hmax <- if (!suppressWarnings(is_empty(x$s_hmax))) {
    x$s_hmax + dataSpecies$s_hmax - mean(dataSpecies$s_hmax)
  }else{dataSpecies$s_hmax}
  
  dataSpecies$s_ah <- if (!suppressWarnings(is_empty(x$s_ah))) {
    x$s_ah + dataSpecies$s_ah - mean(dataSpecies$s_ah)
  }else{dataSpecies$s_ah}
  
  dataSpecies$s_tlp <- if (!suppressWarnings(is_empty(x$s_tlp))) {
    x$s_tlp + dataSpecies$s_tlp - mean(dataSpecies$s_tlp)
  }else{dataSpecies$s_tlp}
  
  return(dataSpecies)
}

.fnForest <- function(x,forestInit){
  simulation <- NULL
  return(forestInit %>% 
    filter(as.integer(as.factor(simulation)) == as.integer(x$ForestID)))
}
