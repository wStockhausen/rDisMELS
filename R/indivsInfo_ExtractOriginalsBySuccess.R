#'
#' @title Extract a dataframe of original IDs by success
#'
#' @description Function to extract a dataframe of original IDs by success.
#'
 #' @param dfrStart - dataframe with startTime and origID of unique initial individuals
 #' @param dfrEnd - dataframe with startTime, orgiID, and id of unique individuals in "successful" life stage(s)
 #'
 #' @return a dataframe with origID and startTime for successful individuals.
 #'
 #' @details The "index" consists of a dataframe with columns
 #' \itemize{
 #'   \item{startTime}
 #'   \item{origID}
 #'   \item{successful - TRUE/FALSE indicating whether or not a "succssful" life stage was eventually reached}
 #' }
 #' and a row for each "original" individual.
 #'
 #' @import dplyr
 #' @import magrittr
 #'
 #' @export
 #'
indivsInfo_ExtractOriginalsBySuccess<-function(dfrStart,
                                               dfrEnd){
   dfrp<-dfrStart %>%
           dplyr::select(startTime,origID) %>%
           dplyr::left_join(dfrEnd %>% dplyr::select(startTime,origID,id),by=c("startTime","origID")) %>%
           dplyr::mutate(successful=!is.na(id)) %>%
           dplyr::select(startTime,origID,successful) %>%
           dplyr::distinct() %>%
           dplyr::arrange(startTime,origID);
   return(dfrp);
}
