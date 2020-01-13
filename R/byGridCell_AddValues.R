#'
#' @title Add values by grid cell
#'
#' @description Function to add values by grid cell.
#'
#' @param dfr1 - dataframe with results by grid cell
#' @param dfr2 - dataframe with results by grid cell
#' @param cols - dataframe column names to scale
#'
#' @return dataframe with added results
#'
#' @details "Factor" levels (gridCellID, startTime) in dataframes must be aligned, each must have the same number of rows.
#'
#' @export
#'
byGridCell_AddValues<-function(dfr1,
                               dfr2,
                                cols=c("unsuccessful_indivs",   "successful_indivs",   "total_indivs",
                                       "unsuccessful_abundance","successful_abundance","total_abundance")){
  for (col in cols) dfr1[[col]]<-dfr1[[col]]+dfr2[[col]];
  return(dfr1);
}
