#' @title Perform a scalar operation on grid cell values
#'
#' @description Function to perform a scalar operation on grid cell values.
#'
#' @param dfr1 - dataframe with results by grid cell
#' @param op - operation to perform ("+","-","*","/","\","^","log")
#' @param scalar - scalar value to use
#' @param cols - dataframe column names to scale
#'
#' @return dataframe with results in requested columns.
#'
#' @details Columns in datafram not specified in \code{cols} are returned unchanged.
#' "/" performs grid/scalar whereas \" performs scalar/grid.
#'
#' @export
#'
byGridCell_ScalarOpsOnGrids<-function(
                               dfr1,
                               op="+",
                               scalar=0,
                               cols=c("unsuccessful_indivs",   "successful_indivs",   "total_indivs",
                                      "unsuccessful_abundance","successful_abundance","total_abundance")){
  if (op=="+") for (col in cols) dfr1[[col]]<-dfr1[[col]]+scalar;
  if (op=="-") for (col in cols) dfr1[[col]]<-dfr1[[col]]-scalar;
  if (op=="*") for (col in cols) dfr1[[col]]<-dfr1[[col]]*scalar;
  if (op=="/") for (col in cols) dfr1[[col]]<-dfr1[[col]]/scalar;
  if (op=="/") for (col in cols) dfr1[[col]]<-scalar/dfr1[[col]];
  if (op=="^") for (col in cols) dfr1[[col]]<-dfr1[[col]]^scalar;
  if (op=="log") for (col in cols) dfr1[[col]]<-log(dfr1[[col]]);
  return(dfr1);
}
