#'
#'@title Extract info by individual at start and end of life or model run
#'
#'@description Function to extract info by individual at start and end of life or model run.
#'
#'@param sf_start - (optional) \pkg{sf} dataframe returned by \code{\link{indivsInfo_ExtractStart}}
#'@param sf_ebs   - (optional) \pkg{sf} dataframe returned by \code{\link{indivsInfo_ExtractEndByStage}}
#'@param lst_indivs - (req'd if sf_start or sf_end is NULL) list of \pkg{sf} dataframes by life stage returned by \code{\link{indivsInfo_ReorderResults}}
#'@param startLHS   - (req'd if sf_start is NULL) life stage in which all individuals start
#'@param addVars - (req'd if lst_indivs is given) character vector with names of additional (non-default) variables to extract
#'@param checkCalcs - flag (T/F) to check endGeom is correctly assigned (for debugging)
#'
#'@return \pkg{sf} dataframe with columns:
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
#'  \item{start..AddVars - starting values for additional (non-default) variables}
#'  \item{startGeom - starting 2d location as SF_POINT}
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
#'  \item{end..AddVars - ending values for additional (non-default) variables}
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement)}
#'  \item{endGeom - ending 2d location as SF_POINT}
#'}
#'
#'@details The input \pkg{sf} dataframes \code{sf_start} and \code{sf_end} should be
#'the output of \code{\link{indivsInfo_ExtractStart}} and  \code{\link{indivsInfo_ExtractEnd}},
#'respectively. If both are given as an inputs, \code{addVars} is ignored.
#'
#'If either are NULL, \code{lst_indivs} must be given. If \code{sf_start} is NULL,
#'\code{startLHS} must be given. If \code{sf_start} is NULL, it is calculated internally
#'using \code{sf_start = indivsInfo_ExtractStart(lst_indivs[[startLHS]],addVars=addVars)}.
#'
#'If \code{sf_end}
#'is NULL, it is calculated internally using \code{sf_ebs = indivsInfo_ExtractEndByStage(lst_indivs,addVars=addVars)}
#'followed by \code{sf_end = indivsInfo_ExtractEnd(sf_ebs)}.
#'
#'@import dplyr
#'@import magrittr
#'@import tibble
#'
#'@export
#'
indivsInfo_ExtractStartEndByStage<-function(sf_start=NULL,
                                            sf_ebs=NULL,
                                            lst_indivs=NULL,
                                            startLHS=NULL,
                                            addVars="",
                                            checkCalcs=FALSE){
  #--process lst_indvs, as necessary
  if (is.null(sf_start)) sf_start = indivsInfo_ExtractStart(lst_indivs[[startLHS]],addVars=addVars);
  if (is.null(sf_ebs))   sf_ebs   = indivsInfo_ExtractEndByStage(lst_indivs,addVars=addVars);

  #--note: can't join two sf dataframes on attributes.
  #--Could do either of the following:
  #--1. drop geometry column (endGeoms) in sf_ebs in order to do right join, then add it back on.
  #------In this case, need to make sure, though, that endGeoms match to
  #------correct individuals in the combined dataframe.
  #--2. convert sf_ebs to tibble in order to do right join.
  #------In this case, endGeom is converted to a list column and not dropped from sf_ebs in inner join
  #--Approach 1 was initially implemented, but approach 2 is simpler and is now implemented.

  #----Approach 1
  # #----create combined sf dataframe (missing endGeom column, though) with same order as sf_ebs
  # sf_start %<>% dplyr::arrange(startTime,origID);             #--make sure sf_start is in known order
  # sf_ebs   %<>% dplyr::arrange(startTime,origID,endID,endAge);#--make sure sf_ebs is in known order
  # sf_sebs = sf_start %>%
  #             dplyr::select(!successful) %>%
  #             dplyr::right_join(sf_ebs %>% sf::st_drop_geometry(),by=c("startTime","origID")) %>%
  #             dplyr::arrange(startTime,origID,endID,endAge);
  # sf_sebs %<>% dplyr::mutate(endGeom=sf_ebs$endGeom);#--add in endGeoms
  # if (checkCalcs){
  #   #--check the above is correct
  #   cnt = sum((sf_sebs$startTime!=sf_ebs$startTime)|(sf_sebs$origID!=sf_ebs$origID)|
  #             (sf_sebs$endID!=sf_ebs$endID)|(sf_sebs$endAge!=sf_ebs$endAge));
  #   if (cnt==0){
  #     cat("Info: checksum in rDisMELS::indivsInfo_ExtractStartEndByStage was zero, as expected.\n")
  #   } else {
  #     str = "Checksum in rDisMELS::indivsInfo_ExtractStartEndByStage was not zero!"
  #     warning(str, immediate.=TRUE);
  #   }
  # }

  #--Approach 2
  str = "sf_sebs = sf_start %>% &&successful
                     dplyr::right_join(tibble::as_tibble(sf_ebs),by=c('startTime','origID')) %>%
                     dplyr::arrange(startTime,origID,endID,endAge);";
  if (hasSuccessful) {
    #--drop 'successful' column from sf_start, will pick up from sf_ebs
    str = gsub("&&successful","dplyr::select(!successful) %>%",str,fixed=TRUE);
  } else {
    #--no 'successful' column to drop
    str = gsub("&&successful","",str,fixed=TRUE);
  }
  if (verbose) message(str);
  eval(parse(text=str)[[1]]);

  return(sf_sebs);
}
#sf_sebs = indivsInfo_ExtractStartEndByStage(sf_start,sf_ebs,checkCalcs=TRUE);
#sf_sebs = indivsInfo_ExtractStartEndByStage(lst_indivs=lst_indivs,startLHS=startLHS,checkCalcs=TRUE);
#sf_sebs = indivsInfo_ExtractStartEndByStage(sf_start,sf_ebs,checkCalcs=TRUE,addVars=c("temperature","salinity"));
#sf_sebs = indivsInfo_ExtractStartEndByStage(lst_indivs=lst_indivs,startLHS=startLHS,checkCalcs=TRUE,addVars=c("temperature","salinity"));

