#'
#' @title Perform an algebraic operation on values by grid cell in two dataframes
#'
#' @description Function to perform an algebraic operation on values by grid cell in two dataframes.
#'
#' @param dfr1 - dataframe with results by grid cell
#' @param op - operation by grid cell ("+","-","*","/")
#' @param dfr2 - dataframe with results by grid cell
#' @param cols - dataframe column names to add together
#'
#' @return dataframe (dfr1) with results of operation in requested columns.
#'
#' @details "Factor" levels (gridCellID, startTime) in dataframes must be aligned,
#' each must have the same number of rows.#' Columns in datafram not specified in
#' \code{cols} are returned unchanged.
#'
#' @export
#'
byGridCell_GridAlgebra<-function(dfr1,
                                 op,
                                 dfr2,
                                 cols=c("unsuccessful_indivs",   "successful_indivs",   "total_indivs",
                                        "unsuccessful_abundance","successful_abundance","total_abundance")){
  if (nrow(dfr1)!=nrow(dfr2)){
    stop("rDisMELS::byGridCell_AddValues: number f rows must be equal for both dataframes\n")
  }
  if (op=="+") for (col in cols) dfr1[[col]]<-dfr1[[col]]+dfr2[[col]];
  if (op=="-") for (col in cols) dfr1[[col]]<-dfr1[[col]]-dfr2[[col]];
  if (op=="*") for (col in cols) dfr1[[col]]<-dfr1[[col]]*dfr2[[col]];
  if (op=="/") for (col in cols) dfr1[[col]]<-dfr1[[col]]/dfr2[[col]];
  return(dfr1);
}
