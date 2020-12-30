#'
#' @title Extract info on individuals at start of initial life stage
#'
#' @description Function to extract info on individuals at start of initial life stage.
#'
#' @param sf_indivs - \pkg{sf} dataframe with reordered "connectivity results" for the initial life stage
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
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement)}
#'  \item{startGeom - starting 2d location as SF_POINT}
#'}
#'
#'@details The input \pkg{sf} dataframe should be the one returned
#'by \code{\link{reorderAndClassifyResults}} for the initial life stage.
#'For each individual, the initial location is identified by the record
#'when \code{startTime == time}.
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
indivsInfo_ExtractStart<-function(sf_indivs){
  sf_start = sf_indivs %>%
               subset(startTime==time) %>%
               dplyr::select(startTime,origID,typeName,
                             gridCellID,horizPos1,horizPos2,vertPos,bathym,
                             age,number,successful,
                             geom) %>%
               dplyr::rename(startLHS=typeName,
                             startGridCell=gridCellID,startLon=horizPos1,startLat=horizPos2,
                             startDepth=vertPos,startBathym=bathym,
                             startAge=age,startNum=number,
                             startGeom=geom);
  return(sf_start);
}
#sf_start = indivsInfo_ExtractStart(sf_indivs);
