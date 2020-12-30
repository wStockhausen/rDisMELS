#'
#'@title Extract age for each individual at end of life or model run
#'
#'@description Function to extract age for each individual at end of life or model run.
#'
#' @param sf_EndByStage - \pkg{sf} dataframe returned by \code{\link{indivsInfo_ExtractEndByStage}} or \code{\link{indivsInfo_ExtractStartEndByStage}}
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
#'\code{\link{indivsInfo_ExtractEndByStage}} or \code{\link{indivsInfo_ExtractStartEndByStage}}.
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
indivsInfo_ExtractMaxAge<-function(sf_EndByStage){
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
#df_MaxAge = indivsInfo_ExtractMaxAge(sf_ebs);
#df_MaxAge = indivsInfo_ExtractMaxAge(sf_sebs);

