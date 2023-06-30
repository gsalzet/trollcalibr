#' @importFrom dplyr left_join mutate row_number select filter if_else arrange
#' @importFrom SLHD maximinSLHD
#' @importFrom ulid generate
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @importFrom purrr map
#' @importFrom rlang is_empty
NULL
#' Design of experiments for TROLL simulations
#' 
#' Function to generate optimised design of experiments (DOE). 
#' User have to provide a formatted data.frame to define the DOE dimensions 
#' and unscale conversion to texted parameters.
#' 
#' @param paramsBounds d.f. a formatted data.frame (see details) to 
#' describe parameters space.
#' @param forestInit d.f. a troll forest format data.frame indexed 
#' with a unique 'simulation' column.
#' @param ntotalsim int. Total number of simulations per forest point.
#' @param sequential bool. Selection of a sequential design 
#' based on gaussian processes emulation per forest point.
#' @param ninitsim int. Initial number of simulations if 
#' sequential design is selected.
#' @param name chr. Name of design and key for simulation.
#' If not provided, a ULID key is generated (see \code{ulid::\link[ulid:generate]{generate}} for details).
#' @param path char. Path to save the simulation outputs, 
#' the default is null corresponding to a simulation in memory without saved intermediary files.
#' @param nreplica int. Number of replica per sampling point (default: 10).
#' @param corrmat mat. Pearson correlation matrix between studied parameters. 
#' If not provided, uncorrelated matrix will be used.
#' @param echo bool. display information messages (default = TRUE).
#' @param maxit Maximum number of iterations to compute correlation.
#' Set to 0 to use a heuristic based on the size of the hypercube.
#' Set to a negative number to never give up. *CAUTION*, this might result in an infinite loop.
#' @param dimPlot list. Plot dimension with a list of xdim and ydim (in m).
#' If forestInit is provided, 'dimPlot' must be a named list of list of xdim and ydim.
#'
#' @details
#' 'paramsBounds' data.frame is formatted as followed:
#'  * parameter: chr. Name of the parameter within TROLL settings, 
#'  covariate table or experiment setup ;
#'  * quantileFn: fct. Quantile function to scale margin 
#'  distribution of studied parameter ;
#'  * type: chr. Type of parameter ('global'/ 'species'/ 
#'  'climate'/ 'daily'/ 'covariate/ 'experiment','qualitative').
#'  
#'  'forestInit' data.frame is formatted as followed:
#'  * simulation: chr. Unique identifier for each studied plot 
#'  (common between tree within plot) ;
#'  * col: int. y-axis of tree location ;
#'  * row: int. x-axis of tree location
#'  * s_name: chr. tree specie name ;
#'  * CrownDisplacement: 	int. allowed crown plasticity (set to 0) ;
#'  * Pmass: P mass in leaf (in g.m-2) ;
#'  * Nmass: N mass in leaf (in g.m-2) ;
#'  * LMA: Leaf Mass Area (in g.m-2) ;
#'  * wsg: Wood Specific Gravity (in kg.m-3);
#'  *	dbhmax: Maximum diameter at breast height (in m) ;
#'  * dbh:	 Current diameter at breast height (in m) ;
#'  * height:	Current tree height (in m) ;
#'  * CD: Crown depth (in m) ;	
#'  * CR: Crown radius (in m).
#' 
#'
#' @return a trolldae object.
#' @export
#'
#' @examples
#' 
#'   require(tibble)
#'   require(dplyr)
#'   
#'  paramsBounds <- tibble(parameter = c("klight","phi","g1","nbs0","CR_a","CR_b","Hmaxcor","habitat"),
#'   quantileFn = c(\(x) {qunif(x,0.5,0.95)}, # klight
#'    \(x) {qunif(x,0.04,0.09)}, # phi
#'    \(x) {qunif(x,2,5)}, # g1
#'    \(x) {ceiling(10^qunif(x,0,3))}, # log10nbs0
#'    \(x) {qunif(x,1.5,3)}, # CR_a
#'    \(x) {qunif(x,0.4,0.8)}, # CR_b
#'    \(x) {qunif(x,0.8,1.2)},# Hmaxcor
#'    \(x) {ceiling(qunif(x,0,14))}), #Habitat 
#'    type = c("global","global","global","global","global","global","species", "qualitative"))
#'    
#' ForestTest <- TROLLv3_sim@forest
#' ForestInitTRUE <- rbind(ForestTest %>% mutate(simulation = "test"),
#' ForestTest %>% mutate(simulation = "test1"))
#' 
#' setupDesign(paramsBounds = paramsBounds,ntotalsim = 10,echo = FALSE,forestInit = ForestInitTRUE,
#' dimPlot = list("test" = list("xdim" = 100,"ydim" = 100),
#' "test1" = list("xdim" = 100,"ydim" = 100)))
#' 
setupDesign <- function(name = NULL,
                        paramsBounds,
                        forestInit = NULL,
                        ntotalsim,
                        ninitsim = NULL,
                        sequential = FALSE,
                        path = NULL,
                        nreplica = 10,
                        corrmat = NULL,
                        echo = FALSE,
                        maxit = NULL,
                        dimPlot = list("xdim" = 250L,
                                       "ydim" = 250L)
                        ) {
  
  # Arguments check
  
  if (!inherits(paramsBounds, "data.frame")) {
    stop("'paramsBounds' argument of 'setupDesign' must be a data.frame")
  }
  if (!inherits(forestInit, c("data.frame","NULL"))) {
    stop("'forestInit' argument of 'setupDesign' must be a data.frame")
  }
  if (!inherits(ntotalsim, c("numeric","integer") ) | !(as.integer(ntotalsim) == ntotalsim) & !is.null(ntotalsim)) {
    stop("'ntotalsim' argument of 'setupDesign' must be a integer")
  }
  if (!all(unlist(lapply(list(name, path), inherits, c("character","NULL") )))) {
    stop("'name'and 'path' arguments of 'setupDesign' must be a character and dir in path must exist")
  }
  if (!inherits(corrmat, c("matrix","NULL"))) {
    stop("'corrmat' argument of 'setupDesign' must be a matrix")
  }
  
  if (!inherits(dimPlot, "list")) {
    stop("'dimPlot' argument of 'setupDesign' must be a list")
  }
  
  if(!all(unlist(lapply(list(ninitsim, nreplica, maxit), inherits, c("numeric","integer") )))){
    test1 <- test2 <- test3 <- FALSE
    if (!is.null(ninitsim)) {
      test1 <- !(as.integer(ninitsim) == ninitsim)
    }
    if (!is.null(nreplica)) {
      test2 <- !(as.integer(nreplica) == nreplica)
    }
    if (!is.null(maxit)) {
      test3 <- !(as.integer(maxit) == maxit)
    }
    if(any(c(test1,test2,test3))){
      stop("'ninitsim', 'nreplica' and 'maxit' arguments of 'setupDesign' must be an integer")
    }
    
  }
  
  # Global variables
  type <- nsim <- nparam <- X <- NULL
  default_forest <- default_paramsBounds <- NULL
  forestID <- parameter <- NULL
  # Arguments validity check
  
  if (!is.null(forestInit)) {
    default_forest <- c("simulation","col", "row",
                        "s_name",	"CrownDisplacement",	
                        "Pmass",	"Nmass",	"LMA",
                        "wsg",	"dbhmax",		"dbh",	
                        "height",	"CD",	"CR")
    
    if (!all(default_forest %in% colnames(forestInit))) {
      stop("'forestInit' argument of 'setupDesign' is not formatted as detailled in 'setupDesign' help")
    }
    
    forestID <- unique(forestInit$simulation)
    if (!all(forestID %in% names(dimPlot))) {
      stop(" 'dimPlot' and 'forestInit' arguments of 'setupDesign' does not share the same 'simulation' identifier")
    }
    for(i in 1:length(dimPlot)){
      if (!all(names(dimPlot[[i]]) %in% c("xdim","ydim")) | is_empty(dimPlot[[i]])) {
        stop(" 'dimPlot' argument of 'setupDesign' is not formatted as a list of integer with 'xdim' and 'ydim'")
      }
    }
  }else{
    if (!all(names(dimPlot) %in% c("xdim","ydim"))) {
      stop(" 'dimPlot' argument of 'setupDesign' is not formatted as a list of integer with 'xdim' and 'ydim'")
    }
  }
  
  
  default_paramsBounds <- c("parameter","quantileFn","type")
  if (!all(default_paramsBounds %in% colnames(paramsBounds))) {
    stop("'paramsBounds' data.frame argument of 'setupDesign' is not formatted as detailled in 'setupDesign' help")
  }
  
  if (!all(default_paramsBounds %in% colnames(paramsBounds))) {
    stop("'paramsBounds' data.frame argument of 'setupDesign' is not formatted as detailled in 'setupDesign' help")
  }
  
  types_variable <-  c("global", "species", "climate", "daily", "covariate","qualitative" ,"experiment")
  
  if (!all(paramsBounds$type %in%  types_variable)) {
    stop("'paramsBounds' data.frame argument of 'setupDesign' is not formatted with correct 'type'")
  }
  
  # ------------------
  
  
  
  if (is.null(name)) { 
    name <- paste0("`",generate(),"`")
  }
  
  if (is.null(path)) {
    path <- tempdir()
  }
  
  if (sequential && !is.null(ninitsim)) {
    nsim <-  ninitsim
    type <-  c("HM", "GP","RAW")
  }else{
    nsim <- ntotalsim
    type <- "RAW"
  }
  
  
  namesparamsinit <- paramsBounds$parameter
  
  paramsBounds <- paramsBounds %>%  arrange(type,parameter)
  
  namesparams <- paramsBounds$parameter
  
  nparam <- dim(paramsBounds)[1]

  if(is.null(maxit)){
    maxit <- 0
  }
  
  if (nsim < 10*nparam & echo) {
    message("'ninitsim' argument of 'setupDesign' is not >= 10*Parameters.")
  }
  
  if (is.null(corrmat) & 
      any(namesparams %in% "CR_a") & 
      any(namesparams %in% "CR_b")) {
    corrmat <- diag(nparam)
    
    corrmat[which(paramsBounds$parameter == "CR_a"),
            which(paramsBounds$parameter == "CR_b")] <- 0.65
    
    corrmat[which(paramsBounds$parameter == "CR_b"),
            which(paramsBounds$parameter == "CR_a")] <- 0.65
  }else{
    
    if (is.null(corrmat)) {
      corrmat <- diag(nparam)
    }
   
  }
  
  Order <- do.call(c,lapply(1:length(namesparams),function(x){which(namesparams[x] == namesparamsinit) }))
  
  corrmat <- corrmat[Order,Order]
  
  if (echo) {
    message("Computing maximin LHS")
  }
  
  Nqualitative <- dim(paramsBounds[which(paramsBounds$type == "qualitative"),])[1]
  
  if (Nqualitative > 0 | !is.null(forestInit)) {
    
    if(!is.null(forestInit)){
      paramsBounds <- rbind(tibble(parameter = c("forestInit"),
                             quantileFn = c(
                               function(x){floor(qunif(x,1,length(unique(forestInit$simulation))))}#"forestInit"
                             ),
                             type = c("qualitative")),paramsBounds)
    }
    
    Nqualitative <- dim(paramsBounds[which(paramsBounds$type == "qualitative"),])[1]
    
    Quali <- paramsBounds[which(paramsBounds$type == "qualitative"),]
    
    Mat_quali <- expand.grid(lapply(1:Nqualitative,function(x){seq(Quali$quantileFn[x][[1]](0),
                                                                   Quali$quantileFn[x][[1]](1),1)}))
    
    colnames(Mat_quali) <- Quali$parameter
    
    NcombiQuali <- dim(Mat_quali)[1]
    
    if (nparam - Nqualitative > 0) {
      NotQuali <- paramsBounds[which(paramsBounds$type != "qualitative"),]
      LHS <- maximinSLHD(t = max( NcombiQuali,1),m = floor(nsim/max(NcombiQuali,1)),k = max(dim(NotQuali)[1],1),itermax = max(100,maxit))
      X <- LHS$StandDesign
      Xquanti <- X[,2:(dim(NotQuali)[1]+1)]
      colnames(Xquanti) <- NotQuali$parameter
      
      Xquali <- do.call(rbind, lapply(1:dim(X)[1],function(x){ 
        Mat_quali[X[x,1],]}))
      
      Xquali <- do.call(cbind,lapply(1:Nqualitative,function(x){
        Xquali[,x]/Quali$quantileFn[x][[1]](1)}))
      
      colnames(Xquali) <- Quali$parameter
      
      X <- cbind(Xquanti,Xquali) %>% as_tibble() %>% select(paramsBounds$parameter) %>% 
        as.matrix()
      
    }else{
      X <- do.call(cbind,lapply(1:Nqualitative,function(x){
        Mat_quali[,x]/Quali$quantileFn[x][[1]](1) - 0.5/Quali$quantileFn[x][[1]](1)}))
    }
    
    
    paramsBounds$type[paramsBounds$type == "qualitative"] <- "covariate"
  }else{
    NcombiQuali <- 0
    LHS <- maximinSLHD(t = 1,m = nsim,k = nparam,itermax = max(100,maxit))
    X <- LHS$StandDesign
  }

  
  if (!all(corrmat == diag(nparam))) {
    if (echo) {
      message("Computing correlated LHS")
    }
    
    if (!is.null(forestInit)) {
      corrmat <- cbind(c(1,rep(0,nparam)),rbind(rep(0,nparam),corrmat))
      X <- .lhscorcorr(X,cormat = corrmat,eps = 0.001*sqrt(nsim), echo = echo,maxit = 1E10)
      nparam <- nparam +1
    }else{
      X <-.lhscorcorr(X,cormat = corrmat,eps = 0.001*sqrt(nsim), echo = echo,maxit = 1E10)
    }
    
  }
  
  X <- rbind(as.matrix(X), matrix(rep(t(X), max(nreplica -1, 0) ), ncol= nparam , byrow=TRUE))
  
  
  X <- X[sample(nrow(X)),]#shuffle rows with added replica
  
  if(!is.null(forestInit)){
    colnames(X) <- c("ForestID",namesparams)
  }else{
    colnames(X) <- namesparams
  }
  
  
  
  return(trolldae(name = name,
                  state = "Generated",
                  lhs = X,
                  boundaries = paramsBounds,
                  doeopts = list("ntotalsim" = ntotalsim,
                                 "nsim" = nsim,
                                 "nreplica" = nreplica, 
                                 "corrmat" = corrmat,
                                 "fnParams" = list("global" = NULL,
                                                   "climate" = NULL,
                                                   "daily" = NULL,
                                                   "lidar" = NULL),
                                 "forestInit" = forestInit,
                                 "dimPlot" = if(!is.null(forestInit)){dimPlot}else{list(dimPlot)}),
                  path = path,
                  type = type)
  )
}


