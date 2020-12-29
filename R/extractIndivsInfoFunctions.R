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
extractIndivInfo_Start<-function(sf_indivs){
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
#sf_start = extractIndivInfo_Start(sf_indivs);

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
extractIndivInfo_EndByStage<-function(sfs_indivs){
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
#sf_EndByStage = extractIndivInfo_EndByStage(sfs_indivs);

#'
#'@title Extract info by individual at start and end of life or model run
#'
#'@description Function to extract info by individual at start and end of life or model run.
#'
#'@param sf_start - \pkg{sf} dataframe returned by \code{\link{extractIndivInfo_Start}}
#'@param sf_ebs   - \pkg{sf} dataframe returned by \code{\link{extractIndivInfo_EndByStage}}
#'@param lst_indivs - list of \pkg{sf} dataframes by life stage returned by \code{\link{reorderAndClassifyResults}} (req'd if sf_start or sf_end is NULL)
#'@param startLHS   - life stage in which all individuals start (req'd if sf_start is NULL)
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
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement)}
#'  \item{endGeom - ending 2d location as SF_POINT}
#'}
#'
#'@details The input \pkg{sf} dataframes \code{sf_start} and \code{sf_end} should be
#'the output of \code{\link{extractIndivInfo_Start}} and  \code{\link{extractIndivInfo_End}},
#'respectively. If either are NULL, \code{lst_indivs} must be given. If \code{sf_start} is NULL,
#'\code{startLHS} must be given. If \code{sf_start} is NULL, it is calculated internally
#'using \code{sf_start = extractIndivInfo_Start(lst_indivs[[startLHS]])}. If \code{sf_end}
#'is NULL, it is calculated internally using \code{sf_ebs = extractIndivInfo_EndByStage(lst_indivs)}
#'followed by \code{sf_end = extractIndivInfo_End(sf_ebs)}.
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
extractIndivInfo_StartEndByStage<-function(sf_start=NULL,
                                           sf_ebs=NULL,
                                           lst_indivs=NULL,
                                           startLHS=NULL,
                                           checkCalcs=FALSE){
  #--process lst_indvs, as necessary
  if (is.null(sf_start)) sf_start = extractIndivInfo_Start(lst_indivs[[startLHS]]);
  if (is.null(sf_ebs))   sf_ebs   = extractIndivInfo_EndByStage(lst_indivs);

  #--note: can't join two sf dataframes on attributes.
  #--need to drop geometry column in sf_ebs in order to do right join,
  #--then add it back on. Need to make sure, though, that endGeoms match to
  #--correct individuals in combined dataframe.
  sf_start %<>% dplyr::arrange(startTime,origID);             #--make sure sf_start is in known order
  sf_ebs   %<>% dplyr::arrange(startTime,origID,endID,endAge);#--make sure sf_ebs is in known order
  #----create combined sf dataframe (missing endGeom column, though) with same order as sf_ebs
  sf_sebs = sf_start %>%
              dplyr::select(!successful) %>%
              dplyr::right_join(sf_ebs %>% sf::st_drop_geometry(),by=c("startTime","origID")) %>%
              dplyr::arrange(startTime,origID,endID,endAge);
  sf_sebs %<>% dplyr::mutate(endGeom=sf_ebs$endGeom);#--add in endGeoms
  if (checkCalcs){
    #--check the above is correct
    cnt = sum((sf_sebs$startTime!=sf_ebs$startTime)|(sf_sebs$origID!=sf_ebs$origID)|
              (sf_sebs$endID!=sf_ebs$endID)|(sf_sebs$endAge!=sf_ebs$endAge));
    if (cnt==0){
      cat("Info: checksum in rDisMELS::extractIndivInfo_StartEndByStage was zero, as expected.\n")
    } else {
      str = "Checksum in rDisMELS::extractIndivInfo_StartEndByStage was not zero!"
      warning(str, immediate.=TRUE);
    }
  }
  return(sf_sebs);
}
#sf_sebs = extractIndivInfo_StartEndByStage(sf_start,sf_ebs,checkCalcs=TRUE);
#sf_sebs = extractIndivInfo_StartEndByStage(lst_indivs=lst_indivs,startLHS=startLHS,checkCalcs=TRUE);

#'
#'@title Extract age for each individual at end of life or model run
#'
#'@description Function to extract age for each individual at end of life or model run.
#'
#' @param sf_EndByStage - \pkg{sf} dataframe returned by \code{\link{extractIndivInfo_EndByStage}} or \code{\link{extractIndivInfo_StartEndByStage}}
#'
#'@return \pkg{tibble} dataframe with columns:
#'\itemize{
#'  \item{startTime}
#'  \item{origID - original (starting) individual ID}
#'  \item{endID - individual ID at end}
#'  \item{maxAge - ending age(d)}
#'}
#'
#'@details The input \pkg{sf} dataframe should be the output of
#'\code{\link{extractIndivInfo_EndByStage}} or \code{\link{extractIndivInfo_StartEndByStage}}.
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
extractIndivInfo_MaxAge<-function(sf_EndByStage){
  #--determine max age for each individual
  df_MaxAge = sf_EndByStage %>%
                sf::st_drop_geometry() %>%
                dplyr::group_by(startTime,origID,endID) %>%
                dplyr::summarize(maxAge=max(endAge,na.rm=TRUE)) %>%
                dplyr::ungroup();

  #--eliminate extra counting of individuals which changed id
  df_idC  = df_MaxAge %>% subset(!(origID==endID));      #--individuals which changed id
  origIDs = sort(unique(df_idC$origID));                 #--remove multiple-counting
  df_idNC = df_MaxAge %>% subset(!(origID %in% origIDs));#--individuals which did not change id
  df_MaxAge = rbind(df_idC,df_idNC);
  return(df_MaxAge);
}
#df_MaxAge = extractIndivInfo_MaxAge(sf_ebs);
#df_MaxAge = extractIndivInfo_MaxAge(sf_sebs);

#'
#'@title Extract info by individual at end of life or model run
#'
#'@description Function to extract info by individual at end of life or model run.
#'
#' @param sf_EndByStage - \pkg{sf} dataframe returned by \code{\link{extractIndivInfo_EndByStage}}
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
#'@details The input \pkg{sf} dataframe should be the output of
#'\code{\link{extractIndivInfo_EndByStage}}.
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
extractIndivInfo_End<-function(sf_EndByStage){
  #--determine max age for each individual
  df_MaxAge = extractIndivInfo_MaxAge(sf_EndByStage);

  #--determine selected attributes for each individual at max age
  sf_end = sf_EndByStage %>%
             dplyr::inner_join(df_MaxAge,by=c("startTime","origID","endID","endAge"="maxAge"));

  return(sf_end);
}
#sf_end = extractIndivInfo_End(sf_EndByStage);

#'
#'@title Extract info by individual at start and end of life or model run
#'
#'@description Function to extract info by individual at start and end of life or model run.
#'
#'@param sf_sebs  - \pkg{sf} dataframe returned by \code{\link{extractIndivInfo_StartEndByStage}}
#'@param sf_start - \pkg{sf} dataframe returned by \code{\link{extractIndivInfo_Start}}
#'@param sf_end   - \pkg{sf} dataframe returned by \code{\link{extractIndivInfo_End}}
#'@param lst_indivs - list of \pkg{sf} dataframes by life stage returned by \code{\link{reorderAndClassifyResults}} (req'd if sf_ebs and sf_start or sf_end is NULL)
#'@param startLHS   - life stage in which all individuals start (req'd if sf_start is NULL)
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
#'  \item{successful - flag indicating "success" (TRUE) or failure (FALSE) (e.g., settlement)}
#'  \item{endGeom - ending 2d location as SF_POINT}
#'}
#'
#'@details If provided, the input \pkg{sf} dataframe \code{sf_sebs} should be the
#'output of \code{\link{extractIndivInfo_StartEndByStage}}. If \code{sf_sebs} is NULL,
#'the input \pkg{sf} dataframes \code{sf_start} and \code{sf_end} can be given and should be
#'the output of \code{\link{extractIndivInfo_Start}} and  \code{\link{extractIndivInfo_End}},
#'respectively. If either of these are NULL, \code{lst_indivs} must be given. If \code{sf_start} is NULL,
#'\code{startLHS} must be given. If \code{sf_start} is NULL, it is calculated internally
#'using \code{sf_start = extractIndivInfo_Start(lst_indivs[[startLHS]])}. If \code{sf_end}
#'is NULL, it is calculated internally using \code{sf_ebs = extractIndivInfo_EndByStage(lst_indivs)}
#'followed by \code{sf_end = extractIndivInfo_End(sf_ebs)}.
#'
#'@import dplyr
#'@import magrittr
#'
#'@export
#'
extractIndivInfo_StartEnd<-function(sf_sebs=NULL,
                                    sf_start=NULL,
                                    sf_end=NULL,
                                    lst_indivs=NULL,
                                    startLHS=NULL,
                                    checkCalcs=FALSE){
  if (!is.null(sf_sebs)){
    #--simply extract rows where endAge = max(endAge)
    #----determine max age for each individual
    df_MaxAge = extractIndivInfo_MaxAge(sf_ebs);

    #----determine selected attributes for each individual at max age
    sf_se = sf_ebs %>%
               dplyr::inner_join(df_MaxAge,by=c("startTime","origID","endID","endAge"="maxAge"));
    return(sf_se);
  }
  #--else
  if (is.null(sf_start)){
    sf_start = extractIndivInfo_Start(lst_indivs[[startLHS]]);
  }
  if (is.null(sf_end)){
    sf_ebs = extractIndivInfo_EndByStage(lst_indivs);
    sf_end = extractIndivInfo_End(sf_ebs);
  }

  #--note: can't join two sf dataframes on attributes.
  #--need to drop geometry column in sf_end in order to do right join,
  #--then add it back on. Need to make sure, though, that endGeoms match to
  #--correct individuals in combined dataframe.
  sf_start %<>% dplyr::arrange(startTime,origID);      #--make sure sf_start is in known order
  sf_end   %<>% dplyr::arrange(startTime,origID,endID);#--make sure sf_end is in known order
  #----create combined sf dataframe (missing endGeom column, though) with same order as sf_end
  sf_se = sf_start %>%
            dplyr::select(!successful) %>%
            dplyr::right_join(sf_end %>% sf::st_drop_geometry(),by=c("startTime","origID")) %>%
            dplyr::arrange(startTime,origID,endID);
  sf_se %<>% dplyr::mutate(endGeom=sf_end$endGeom);#--add in endGeoms
  if (checkCalcs){
    #--check the above is correct
    cnt = sum((sf_se$startTime!=sf_end$startTime)|(sf_se$origID!=sf_end$origID)|(sf_se$endID!=sf_end$endID));
    if (cnt==0){
      cat("Info: checksum in rDisMELS::extractIndivInfo_StartEnd was zero, as expected.\n")
    } else {
      str = "Checksum in rDisMELS::extractIndivInfo_StartEnd was not zero!"
      warning(str, immediate.=TRUE);
    }
  }
  return(sf_se);
}
#sf_se = extractIndivInfo_StartEnd(sf_ebs=sf_sebs,checkCalcs=TRUE);
#sf_se = extractIndivInfo_StartEnd(sf_start=sf_start,sf_end=sf_end,checkCalcs=TRUE);


