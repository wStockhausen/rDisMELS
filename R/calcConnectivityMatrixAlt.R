#'
#' @title Calculate a connectivity matrix dataframe classified by start/end zones
#'
#' @description Function to calculate a connectivity matrix dataframe individuals classified by start/end zones.
#'
#' @param dfr_isezs - dataframe with individuals classified by start/end zones
#' @param endLHSs - name(s) of ending life stage(s) for connectivity matrix
#' @param startZones - vector of all starting zone id's (integers)
#' @param endZones - vector of all ending zone id's (integers)
#'
#' @return connectivity matrix as a dataframe
#'
#' @details Calculates the connectivity matrix for a model run based on individuals previously
#' classified by start and end zones (e.g., from \code{\link{indivsInfo_ExtractStartEndByStage}}
#' or \code{\link{indivsInfo_ExtractStartEnd}}, followed by \code{\link{indivsInfo_AssignZones}}).
#'
#' This function provides an alternative path to calculating the same connectivity matrices
#' as \code{\link{calcConnectivityMatrix}}.
#'
#' @importFrom sf st_drop_geometry
#'
#' @import dplyr
#' @import magrittr
#' @import tibble
#'
#' @export
#'
calcConnectivityMatrixAlt<-function(dfr_isezs,
                                    endLHSs=c("C1F","C1M"),
                                    startZones=c(1:8),
                                    endZones=c(1:18)){
  #--if present, drop simple features information
  if (inherits(dfr_isezs,"sf")) dfr_isezs %<>% sf::st_drop_geometry();

  #--create dataframe for complete connectivity matrix
  tblSZs = tibble::tibble(startZone=startZones);
  tblEZs = tibble::tibble(endZone  =endZones);
  connZones  = dplyr::full_join(tblSZs,tblEZs,by=character());
  #--create expanded table with full connectivity matrices for all startTimes
  uSTs   = tibble::tibble(startTime=unique(dfr_isezs$startTime));#--unique startTimes
  uStCZs = dplyr::full_join(uSTs,connZones,by=character());      #--expanded full matrices

  #--extract dataframe with number of starting individuals by startTime and startZone
  #----only select one row for original individual (1st 'summarize')
  #----then sum over origIDs                       (2nd 'summarize')
  #----keep only startZones of interest
  dfr_szs = dfr_isezs %>%
              dplyr::select(startTime,origID,startNum,startZone) %>%
              dplyr::group_by(startTime,origID,startZone) %>%
              dplyr::summarize(startNum=first(startNum),.groups="keep") %>%
              dplyr::group_by(startTime,startZone) %>%
              dplyr::summarize(startNum=sum(startNum,na.rm=TRUE),.groups="keep") %>%
              dplyr::ungroup() %>%
              subset(startZone %in% tblSZs$startZone);
  #--expand dfr_szs to full connectivity matrices for all startTimes
  dfr_conn1 = uStCZs %>%
                dplyr::left_join(dfr_szs,by=c("startTime","startZone"))

  #--extract dataframe with number of ending individuals by startTime, startZone, and endZone
  #----only keep startZones, endZones of interest
  dfr_ezs = dfr_isezs %>% subset(endLHS %in% endLHSs) %>%
              dplyr::select(startTime,startZone,endZone,endNum) %>%
              dplyr::group_by(startTime,startZone,endZone) %>%
              dplyr::summarize(endNum=sum(endNum,na.rm=TRUE),.groups="keep") %>%
              dplyr::ungroup() %>%
              subset((startZone %in% tblSZs$startZone)&(endZone %in% tblEZs$endZone));
  #--expand dfr_ezs to full connectivity matrices for all startTimes
  dfr_conn2 = uStCZs %>%
                dplyr::left_join(dfr_ezs,by=c("startTime","startZone","endZone")) %>%
                dplyr::group_by(startTime,startZone,endZone) %>%
                dplyr::mutate(endNum=wtsUtilities::Sum(endNum)) %>%
                dplyr::ungroup();

  #--join tables dfr_conn1 and dfr_conn2, add fractional connectivity
  dfr_conn = dfr_conn1 %>%
               dplyr::inner_join(dfr_conn2,by=c("startTime","startZone","endZone")) %>%
               dplyr::mutate(connFrac = endNum/startNum);

  return(dfr_conn);
}
