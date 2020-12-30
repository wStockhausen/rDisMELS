#'
#'@title Extract info by individual at end of each life stage
#'
#'@description Function to extract info by individual at end of each life stage.
#'
#' @param sfs_indivs - list of \pkg{sf} dataframes returned by \code{\link{reorderAndClassifyResults}}
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
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement)}
#'  \item{endGeom - ending 2d location as SF_POINT}
#'}
#'
#'@details The input list of \pkg{sf} dataframes should be the one returned
#'by \code{\link{reorderAndClassifyResults}} for the "connectivity results" files.
#'For each individual, the end of the life stage is identified by the record
#'with \code{active==FALSE}, indicating transition to next life stage or death.
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
indivsInfo_ExtractEndByStage<-function(sfs_indivs){
  lhss = names(sfs_indivs);
  #--determine selected attributes for each individual at end of each life stage
  sf_EndByStage  = NULL;
  for (lhs in lhss){
    sf_tmp = sfs_indivs[[lhs]] %>%
                 subset(!active) %>%
                 dplyr::select(startTime,origID,time,id,typeName,
                               gridCellID,horizPos1,horizPos2,vertPos,bathym,
                               age,ageInStage,number,successful,
                               geom) %>%
                 dplyr::rename(endLHS=typeName,endID=id,endTime=time,
                               endCellID=gridCellID,endLon=horizPos1,endLat=horizPos2,
                               endDepth=vertPos,endBathym=bathym,
                               endAge=age,endAgeInStage=ageInStage,endNum=number,
                               endGeom=geom);
    sf_EndByStage = rbind(sf_EndByStage,sf_tmp);
  }
  sf_EndByStage %<>% group_by(startTime,origID,endID) %>% arrange(endAge,.by_group=TRUE);
  return(sf_EndByStage);
}
#sf_EndByStage = indivsInfo_ExtractEndByStage(sfs_indivs);

