#'
#'@title Extract info by individual at end of life or model run
#'
#'@description Function to extract info by individual at end of life or model run.
#'
#' @param sf_EndByStage - \pkg{sf} dataframe returned by \code{\link{indivsInfo_ExtractEndByStage}}
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
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement) [if column in \code{sf_EndByStage}]}
#'  \item{endGeom - ending 2d location as SF_POINT}
#'}
#'
#'@details The input \pkg{sf} dataframe should be the output of \code{\link{indivsInfo_ExtractEndByStage}}.
#'
#'For each unique individual, the end of the model run or death is identified by the record
#'where \code{age==max(age)}. "Unique" individuals are determined by eliminating original
#'individuals which resulted in more than one final individual (e.g., when metamorphosis
#'results in more than one individual).
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
indivsInfo_ExtractEnd<-function(sf_EndByStage){
  #--determine max age for each individual
  df_MaxAge = indivsInfo_ExtractMaxAge(sf_EndByStage);

  #--determine selected attributes for each individual at max age
  sf_end = sf_EndByStage %>%
             dplyr::inner_join(df_MaxAge,by=c("startTime","origID","endID","endAge"="maxAge"));

  return(sf_end);
}
#sf_end = indivsInfo_ExtractEnd(sf_EndByStage);
#sf_end = indivsInfo_ExtractEnd(sf_EndByStage,addVars=c("temperature","salinity"));

