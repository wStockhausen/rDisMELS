#'
#' @title Extract an index for successful individuals
#'
#' @description Function to extract an index for successful individuals.
#'
#' @param dfrs - dataframe with successful individuals or list of dataframes
#' @param typeName - name of life stage defining success
#'
#' @return a dataframe with origID and startTime for successful individuals
#'
#' @details The "index" consists of a dataframe listing the origID and startTime for
#' each successful individual in dfrs (or in dfrs[[typeName]] if dfrs is a list of dataframes).
#'
#' @export
#'
extractIndexForSuccessfulIndivs<-function(dfrs,typeName){
  if (is.data.frame(dfrs)) {
    dfr<-dfrs;
  } else {
    dfr<-dfrs[[typeName]];
  }
  idx<-(dfr$ageInStage==0);
  dfrp<-dfr[idx,c("origID","startTime")];
  names(dfrp)<-c("origID","startTime");
  return(dfrp);
}
