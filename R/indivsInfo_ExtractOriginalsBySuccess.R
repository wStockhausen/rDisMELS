#'
#' @title Extract a dataframe of original IDs classified by success
#'
#' @description Function to extract a dataframe of original IDs classified by success.
#'
 #' @param dfrStart - dataframe with startTime and origID of unique initial individuals
 #' @param dfrEnd - NULL, or dataframe with startTime, orgiID, and id of unique individuals in "successful" life stage(s)
 #'
 #' @return a dataframe
 #'
 #' @details The "index" consists of a dataframe with columns
 #' \itemize{
 #'   \item{startTime}
 #'   \item{origID}
 #'   \item{successful - TRUE/FALSE indicating whether or not a "succssful" life stage was eventually reached}
 #' }
 #' and a row for each "original" individual.
 #'
 #' If \code{dfrEnd} is \code{NULL}, all individuals are classified with success as FALSE.
 #'
 #' @import dplyr
 #' @import magrittr
 #'
 #' @export
 #'
indivsInfo_ExtractOriginalsBySuccess<-function(dfrStart,
                                               dfrEnd=NULL){
   if (!is.null(dfrEnd)){
      dfrp<-dfrStart %>%
              dplyr::select(startTime,origID) %>%
              dplyr::left_join(dfrEnd %>% dplyr::select(startTime,origID,id),by=c("startTime","origID")) %>%
              dplyr::mutate(successful=!is.na(id)) %>%
              dplyr::select(startTime,origID,successful) %>%
              dplyr::distinct() %>%
              dplyr::arrange(startTime,origID);
   } else {
      dfrp<-dfrStart %>%
              dplyr::select(startTime,origID) %>%
              dplyr::distinct() %>%
              dplyr::arrange(startTime,origID) %>%
              dplyr::mutate(successful=FALSE);
   }
   return(dfrp);
}
