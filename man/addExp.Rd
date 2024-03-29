% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addExp.R
\name{addExp}
\alias{addExp}
\alias{addExp,trolldae-method}
\title{Function to add an experimental setup to a trolldae object.}
\usage{
addExp(dae, setupExp, ...)

\S4method{addExp}{trolldae}(dae, setupExp, ...)
}
\arguments{
\item{dae}{trolldae. an initialized trolldae object.}

\item{setupExp}{trollexpsetup. an initialized trollexpsetup}

\item{...}{Unused parameters.}
}
\value{
an initialized trolldae object
}
\description{
Function to add an experimental setup to a trolldae object.
}
\examples{

 require(tibble)
 require(dplyr)

paramsBounds <- tibble(parameter = c("CR_a","CR_b","fraction"),
  quantileFn = c(
   \(x) {qunif(x,1.5,3)}, # CR_a
   \(x) {qunif(x,0.4,0.8)}, # CR_b
   \(x) {qunif(x,0.1,0.9)}# fraction
   ),
   type = c("global","global", "experiment"))
   
DAE <- setupDesign(paramsBounds = paramsBounds,ntotalsim = 20)
   
DAEwithParams <- generate_params(DAE, nyearsInit = 600)

fnExpSum <- function(x,...){
   summary <- as.matrix(data.frame("DBH" = mean(x@forest$dbh),
   "LAI" = mean(x@forest$LAI)))
   return(summary)
   }
   
fnExpFrac <- function(x,parameters,...){
    library(dplyr)
   x@forest <-  x@forest \%>\% sample_frac(parameters$fraction) 
   return(x)
   }  
   
Exp1 <- createExp(id = 1, 
 type = "Inter", deltaT = 1,
 fnExp = fnExpFrac,
 parameters = data.frame("fraction" = 0.5),
 inputs = list())
  
   
Exp2 <- createExp(id = 2, 
 type = "Summary", 
 fnExp = fnExpSum,
 parameters = data.frame(),
 inputs = list())
 
 setupExp12 <- setupExperiments(dae = DAEwithParams,
 listexp = list(Exp1,Exp2),
 inputs = list("exempleInputs" = list())
 )
 
 addExp(DAEwithParams,setupExp12)

}
