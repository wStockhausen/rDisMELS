#'
#'@title Extract info by individual at end of each life stage
#'
#'@description Function to extract info by individual at end of each life stage.
#'
#' @param sfs_indivs - list of \pkg{sf} dataframes returned by \code{\link{indivsInfo_ReorderResults}} for "connectivity results" files
#' @param addVars - character vector with names of additional (non-default) variables to extract
#' @param hasSuccessful - flag indicating that sfs_indivs include a "successful" column
#' @param verbose - flag to print debugging info
#'
#'@return \pkg{sf} dataframe with columns:
#'\itemize{
#'  \item{startTime}
#'  \item{origID - original (starting) individual ID}
#'  \item{endTime - ending time}
#'  \item{endID - ending individual ID}
#'  \item{endLHS - ending life stage}
#'  \item{endGridCell - ID of ending ROMS grid cell}
#'  \item{endLon - ending longitude}
#'  \item{endLat - ending latitude}
#'  \item{endDepth - ending depth (m)}
#'  \item{endBathym - bathymetric depth (m) at ending location}
#'  \item{endAge - ending age(d)}
#'  \item{endNum - ending number}
#'  \item{end..AddVars - columns corresponding to additional variables (names converted to camel case)}
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement) [if \code{hasSuccessful}]=TRUE}
#'  \item{endGeom - ending 2d location as SF_POINT}
#'}
#'
#'@details The input list of \pkg{sf} dataframes should be the one returned
#'by \code{\link{indivsInfo_ReorderResults}} for the "connectivity results" files.
#'
#'@note
#'For each individual, the end of the life stage is identified by the record
#'with \code{ageInStage>0}, indicating transition to next life stage or death.
#'
#'@note
#'If additional variables (\code{addVars}) are requested in the output dataframe,
#'the names are prepended by "end" and converted to camel case.
#'
#'@importFrom stringr str_to_sentence
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
indivsInfo_ExtractEndByStage<-function(sfs_indivs,
                                       addVars="",
                                       hasSuccessful=FALSE,
                                       verbose=FALSE){
  lhss = names(sfs_indivs);
  #--determine selected attributes for each individual at end of each life stage
  sf_EndByStage  = NULL;
  for (lhs in lhss){
    if (verbose) message("Processing ",lhs);
    addVarsp = addVars;
    endAddVars = "";
    str =
      "sf_tmp = sfs_indivs[[lhs]] %>%
                   subset(ageInStage>0) %>%
                   dplyr::select(startTime,origID,time,id,typeName,
                                 gridCellID,horizPos1,horizPos2,vertPos,bathym,
                                 age,ageInStage,number,&&addVars&&successful
                                 geom) %>%
                   dplyr::rename(endLHS=typeName,endID=id,endTime=time,
                                 endCellID=gridCellID,endLon=horizPos1,endLat=horizPos2,
                                 endDepth=vertPos,endBathym=bathym,
                                 endAge=age,endAgeInStage=ageInStage,endNum=number,
                                 &&endAddVars
                                 endGeom=geom);"
    if (addVars[1]!=""){
      #--need to:
      #----1. add guards to vars with spaces in name
      #----2. convert end names to camel case
      addVarsp   = paste0(addGuards(addVars),collapse=",");
      endAddVars = paste0(paste0("end",toCamelCase(addVars),"=",addGuards(addVars),collapse=","),",");
      if (verbose) message("addVarsp = ",addVarsp);
      if (verbose) message("endAddVars = ",endAddVars);
    }
    str = gsub("&&addVars",   addVarsp,str,  fixed=TRUE);
    str = gsub("&&endAddVars",endAddVars,str,fixed=TRUE);
    if (hasSuccessful) {
      str = gsub("&&successful","successful,",str,fixed=TRUE);
    } else {
      str = gsub("&&successful",",",str,fixed=TRUE);
    }
    if (verbose) message(str);
    eval(parse(text=str)[[1]]);
    sf_EndByStage = rbind(sf_EndByStage,sf_tmp);
  }
  sf_EndByStage %<>% dplyr::group_by(startTime,origID,endID) %>% dplyr::arrange(endAge,.by_group=TRUE);
  return(sf_EndByStage);
}
#sf_EndByStage = indivsInfo_ExtractEndByStage(sfs_indivs);
#sf_EndByStage = indivsInfo_ExtractEndByStage(sfs_indivs,addVars=c("temperature","salinity"));

