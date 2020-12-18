#'
#' @title Calculate differences between connectivity matrices
#'
#' @description Function to calculate differences between connectivity matrices.
#'
#' @param tbl_conns - dataframe with connectivity values
#' @param tbl_base - dataframe with base connectivity values
#' @param conns_col - name of column in tbl_conns to difference
#' @param base_col - name of column in tbl_base for differencing
#'
#' @return A dataframe of connectivity matrices idential to \code{tbl_conns}, but
#' with the requested difference values in an additional column, \code{diff}.
#'
#' @details Connectivity values from the base matrix (iei., \code{tbl_base}) are
#' subtracted from corresponding values in \code{tbl_conns} on the basis of matching
#' values in the startZone and endZone columns in each dataframe.
#'
#' Note that \code{tbl_conns} may encapsulate several connectivity matrices. The base
#' matrix will be subtracted from each.
#'
#' @import sqldf sqldf
#'
#' @export
#'
calcConnectivityMatrixDiffs<-function(tbl_conns,
                                      tbl_base,
                                      conns_col="connFrac",
                                      base_col="meanFrac"){
  cols = paste(paste0("t.",names(tbl_conns)),collapse=", ");
  qry  = "select
            &&cols,
            t.&&conns_col - b.&&base_col as diff
          from tbl_conns t, tbl_base b
          where
            t.startZone = b.startZone and
            t.endZone = b.endZone;";
  qry<-gsub("&&cols",cols,qry,fixed=TRUE);
  qry<-gsub("&&conns_col",conns_col,qry,fixed=TRUE);
  qry<-gsub("&&base_col", base_col, qry,fixed=TRUE);
  tbl = sqldf::sqldf(qry);
  return(tbl);
}
