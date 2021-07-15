#'
#'@title Extract info by individual at start and end of life or model run
#'
#'@description Function to extract info by individual at start and end of life or model run.
#'
#'@param sf_sebs  - (optional) \pkg{sf} dataframe returned by \code{\link{indivsInfo_ExtractStartEndByStage}}
#'@param sf_start - (optional) \pkg{sf} dataframe returned by \code{\link{indivsInfo_ExtractStart}}
#'@param sf_end   - (optional) \pkg{sf} dataframe returned by \code{\link{indivsInfo_ExtractEnd}}
#'@param lst_indivs - (req'd if sf_ebs and sf_start or sf_end is NULL) list of \pkg{sf} dataframes by life stage returned by \code{\link{indivsInfo_ReorderResults}}
#'@param startLHS   - (req'd if sf_start is NULL) life stage in which all individuals start
#'@param addVars - (req'd if lst_indivs is given) character vector with names of additional (non-default) variables to extract
#'@param hasSuccessful - flag indicating that input dataframes include a "successful" column
#'@param verbose - flag to print debugging info
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
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement) [optional: depends on processing]}
#'  \item{endGeom - ending 2d location as SF_POINT}
#'}
#'
#'@details If provided, the input \pkg{sf} dataframe \code{sf_sebs} should be the
#'output of \code{\link{indivsInfo_ExtractStartEndByStage}}. If \code{sf_sebs} is given
#'as an input, \code{addVars} is ignored.
#'
#'If \code{sf_sebs} is NULL, the input \pkg{sf} dataframes \code{sf_start} and \code{sf_end}
#'can be given and should be the output of
#'\code{\link{indivsInfo_ExtractStart}} and  \code{\link{indivsInfo_ExtractEnd}}, respectively.
#'
#'
#'If either of these are NULL, \code{lst_indivs} must be given. This should be the list of
#'\pkg{sf} dataframes by life stage returned by \code{\link{indivsInfo_ReorderResults}}.
#'
#'
#'If \code{sf_start} is NULL, \code{startLHS} must be given.
#'If \code{sf_start} is NULL, it is calculated internally using
#'\code{sf_start = indivsInfo_ExtractStart(lst_indivs[[startLHS]],addVars=addVars)}.
#'
#'If \code{sf_end} is NULL, it is calculated internally using
#'\code{sf_ebs = indivsInfo_ExtractEndByStage(lst_indivs,addVars=addVars)} followed by
#'\code{sf_end = indivsInfo_ExtractEnd(sf_ebs)}.
#'
#'@importFrom stringr str_to_sentence
#'@import dplyr
#'@import magrittr
#'@import tibble
#'
#'@export
#'
indivsInfo_ExtractStartEnd<-function(sf_sebs=NULL,
                                     sf_start=NULL,
                                     sf_end=NULL,
                                     lst_indivs=NULL,
                                     startLHS=NULL,
                                     addVars="",
                                     hasSuccessful=FALSE,
                                     verbose=FALSE,
                                     checkCalcs=FALSE){
  if (!is.null(sf_sebs)){
    #--simply extract rows where endAge = max(endAge)
    #----determine max age for each individual
    df_MaxAge = indivsInfo_ExtractMaxAge(sf_sebs);

    #----determine selected attributes for each individual at max age
    sf_se = sf_sebs %>%
               dplyr::inner_join(df_MaxAge,by=c("startTime","origID","endID","endAge"="maxAge"));
    return(sf_se);
  }

  #--else

  if (is.null(sf_start)){
    sf_start = indivsInfo_ExtractStart(lst_indivs[[startLHS]],
                                       addVars=addVars,
                                       hasSuccessful=hasSuccessful,
                                       verbose=verbose);
  }
  if (is.null(sf_end)){
    sf_ebs = indivsInfo_ExtractEndByStage(lst_indivs,
                                          addVars=addVars,
                                          hasSuccessful=hasSuccessful,
                                          verbose=verbose);
    sf_end = indivsInfo_ExtractEnd(sf_ebs);
  }

  #--note: can't join two sf dataframes on attributes.
  #--Could do either of the following:
  #--1. drop geometry column (endGeoms) in sf_end in order to do right join, then add it back on.
  #------In this case, need to make sure, though, that endGeoms match to
  #------correct individuals in the combined dataframe.
  #--2. convert sf_end to tibble in order to do right join.
  #------In this case, endGeom is converted to a list column and not dropped from sf_ebs in inner join
  #--Approach 1 was initially implemented, but approach 2 is simpler and is now implemented.

  # #----Approach 1
  # sf_start %<>% dplyr::arrange(startTime,origID);      #--make sure sf_start is in known order
  # sf_end   %<>% dplyr::arrange(startTime,origID,endID);#--make sure sf_end is in known order
  # #----create combined sf dataframe (missing endGeom column, though) with same order as sf_end
  # sf_se = sf_start %>%
  #           dplyr::select(!successful) %>%
  #           dplyr::right_join(sf_end %>% sf::st_drop_geometry(),by=c("startTime","origID")) %>%
  #           dplyr::arrange(startTime,origID,endID);
  # sf_se %<>% dplyr::mutate(endGeom=sf_end$endGeom);#--add in endGeoms
  # if (checkCalcs){
  #   #--check the above is correct
  #   cnt = sum((sf_se$startTime!=sf_end$startTime)|(sf_se$origID!=sf_end$origID)|(sf_se$endID!=sf_end$endID));
  #   if (cnt==0){
  #     cat("Info: checksum in rDisMELS::indivsInfo_ExtractStartEnd was zero, as expected.\n")
  #   } else {
  #     str = "Checksum in rDisMELS::indivsInfo_ExtractStartEnd was not zero!"
  #     warning(str, immediate.=TRUE);
  #   }
  # }

  #--Approach 2
  str = "sf_se = sf_start %>% &&successful
                   dplyr::right_join(tibble::as_tibble(sf_end),by=c('startTime','origID')) %>%
                   dplyr::arrange(startTime,origID,endID);";
  if (hasSuccessful) {
    #--drop 'successful' column from sf_start, will pick up from sf_end
    str = gsub("&&successful","dplyr::select(!successful) %>%",str,fixed=TRUE);
  } else {
    #--no 'successful' column to drop
    str = gsub("&&successful","",str,fixed=TRUE);
  }
  if (verbose) message(str);
  eval(parse(text=str)[[1]]);

  return(sf_se);
}
#sf_se = indivsInfo_ExtractStartEnd(sf_sebs=sf_sebs,checkCalcs=TRUE);
#sf_se = indivsInfo_ExtractStartEnd(sf_start=sf_start,sf_end=sf_end,checkCalcs=TRUE);
#sf_se = indivsInfo_ExtractStartEnd(sf_ebs=sf_sebs,checkCalcs=TRUE,addVars=c("temperature","salinity"));
#sf_se = indivsInfo_ExtractStartEnd(lst_indivs=sfs_indivs,startLHS="Z1",checkCalcs=TRUE,addVars=c("temperature","salinity"));


