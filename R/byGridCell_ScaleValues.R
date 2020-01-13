#' @title Scale values by grid cell by a multiplicative constant
#'
#' @description Function to scale values by grid cell by a multiplicative constant.
#'
#' @param dfr1 - dataframe with results by grid cell
#' @param scale - multiplicative scale factor
#' @param cols - dataframe column names to scale
#'
#' @return dataframe with scaled results
#'
#' @details None.
#'
#' @export
#'
byGridCell_ScaleValues<-function(dfr1,
                                 scale,
                                 cols=c("unsuccessful_indivs",   "successful_indivs",   "total_indivs",
                                        "unsuccessful_abundance","successful_abundance","total_abundance")){
  for (col in cols) dfr1[[col]]<-scale*dfr1[[col]];
  return(dfr1);
}
