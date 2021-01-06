#'
#'@title Classify an indivsInfo dataframe by individual success
#'
#'@description Function to classify an indivsInfo dataframe by individual success.
#'
#'@param dfrIndivs - dataframe with indivsInfo (e.g., from call to \code{\link{indivsInfo_ReorderResults}})
#'@param dfrSuccess - dataframe from \code{\link{indivsInfo_ExtractOriginalsBySuccess}}
#'
#'@return Dataframe identical to \code{dfrIndivs}, but with a column "successful" indicating
#'whether or not an individual was successful.
#'
#'@details None.
#'
#'@importFrom dplyr left_join
#'
#'@export
#'
indivsInfo_ClassifyBySuccess<-function(dfrIndivs,
                                       dfrSuccess){
  dfr = dfrIndivs %>%
          dplyr::left_join(dfrSuccess,by=c("startTime","origID"));
  return(dfr);
}
