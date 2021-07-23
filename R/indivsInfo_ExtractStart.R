#'
#' @title Extract info on individuals at start of initial life stage
#'
#' @description Function to extract info on individuals at start of initial life stage.
#'
#' @param sf_indivs - \pkg{sf} dataframe for the initial life stage from call to \code{\link{indivsInfo_ReorderResults}}
#' @param addVars - character vector with names of additional (non-default) variables to extract
#' @param hasSuccessful - flag indicating that sfs_indivs include a "successful" column
#' @param verbose - flag to print debugging info
#'
#' @return \pkg{sf} dataframe with columns:
#'\itemize{
#'  \item{startTime}
#'  \item{origID - original (starting) individual ID}
#'  \item{startLHS - starting life stage}
#'  \item{startGridCell - ID of starting ROMS grid cell}
#'  \item{startLon - starting longitude}
#'  \item{startLat - starting latitude}
#'  \item{startDepth - starting depth (m)}
#'  \item{startBathym - bathymetric depth (m) at starting location}
#'  \item{startAge - starting depth}
#'  \item{startNum - starting depth}
#'  \item{startTemp - starting temperature}
#'  \item{start..AddVars - columns corresponding to additional variables (names converted to camel case)}
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement) [if \code{hasSuccessful}]=TRUE}
#'  \item{startGeom - starting 2d location as SF_POINT}
#'}
#'
#'@details The input \pkg{sf} dataframe should be the one returned
#'by \code{\link{indivsInfo_ReorderResults}} for the initial life stage.
#'For each individual, the initial location is identified by the record
#'when \code{startTime == time}.
#'
#'@note
#'If additional variables (\code{addVars}) are requested in the output dataframe,
#'the names are prepended by "end" and converted to camel case.
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
indivsInfo_ExtractStart<-function(sf_indivs,
                                  addVars="",
                                  hasSuccessful=FALSE,
                                  verbose=FALSE){
  addVarsp = addVars;
  startAddVars = "";
  str =
    "sf_start = sf_indivs %>%
                 subset(startTime==time) %>%
                 dplyr::select(startTime,origID,typeName,
                               gridCellID,horizPos1,horizPos2,vertPos,bathym,
                               age,number,&&addVars&&successful
                               geom) %>%
                 dplyr::rename(startLHS=typeName,
                               startGridCell=gridCellID,startLon=horizPos1,startLat=horizPos2,
                               startDepth=vertPos,startBathym=bathym,
                               startAge=age,startNum=number,
                               &&startAddVars
                               startGeom=geom);"
  if (addVars[1]!=""){
    #--need to:
    #----1. add guards to vars with spaces in name
    #----2. convert start names to camel case
    addVarsp     = paste0(addGuards(addVars),collapse=",");
    startAddVars = paste0(paste0("start",toCamelCase(addVars),"=",addGuards(addVars),collapse=","),",");
    if (verbose) message("addVarsp     = ",addVarsp);
    if (verbose) message("startAddVars = ",startAddVars);
  }
  str = gsub("&&addVars",     addVarsp,str,    fixed=TRUE);
  str = gsub("&&startAddVars",startAddVars,str,fixed=TRUE);
  if (hasSuccessful) {
    str = gsub("&&successful","successful,",str,fixed=TRUE);
  } else {
    str = gsub("&&successful",",",str,fixed=TRUE);
  }
  if (verbose) message(str);
  eval(parse(text=str)[[1]]);
  return(sf_start);
}
#sf_start = indivsInfo_ExtractStart(sf_indivs);
#sf_start = indivsInfo_ExtractStart(sf_indivs,addVars=c("temperature","salinity"));
