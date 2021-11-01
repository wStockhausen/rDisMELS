#'
#' @title Perform a mathematical operation on values by grid cell in one or two dataframes
#'
#' @description Function to perform a mathematical operation on values by grid cell in one or two dataframes.
#'
#' @param dfr1 - dataframe with results by grid cell
#' @param op - operation by grid cell ("+","-","*","/","sqrt")
#' @param dfr2 - dataframe with results by grid cell (ignored if op="sqrt")
#' @param cols - dataframe column names to apply operation to
#'
#' @return dataframe with results of operation in requested columns.
#'
#' @details "Factor" levels (gridCellID, startTime) in dataframes must be aligned,
#' each must have the same number of rows. Columns in dataframe not specified in
#' \code{cols} are returned unchanged.
#'
#' Note that dfr1 and dfr2 could also be tibbles or sf datasets. The returned type will be that
#' of \code{dfr1}.
#'
#' @export
#'
byGridCell_GridOps<-function(dfr1,
                             op,
                             dfr2=NULL,
                             cols=c("unsuccessful_indivs",   "successful_indivs",   "total_indivs",
                                    "unsuccessful_abundance","successful_abundance","total_abundance")){
  if (!is.null(dfr2)){
    if (nrow(dfr1)!=nrow(dfr2)){
      stop("rDisMELS::byGridCell_GridOpsOnGrids: number f rows must be equal for both dataframes\n")
    }
  }
  if (op=="+") for (col in cols) dfr1[[col]] = dfr1[[col]]+dfr2[[col]];
  if (op=="-") for (col in cols) dfr1[[col]] = dfr1[[col]]-dfr2[[col]];
  if (op=="*") for (col in cols) dfr1[[col]] = dfr1[[col]]*dfr2[[col]];
  if (op=="/") for (col in cols) dfr1[[col]] = dfr1[[col]]/dfr2[[col]];
  if (op=="sqrt") for (col in cols) dfr1[[col]] = sqrt(dfr1[[col]]);
  return(dfr1);
}
