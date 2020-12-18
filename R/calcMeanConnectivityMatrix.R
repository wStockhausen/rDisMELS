#'
#' @title Calculate mean connectivity matrix
#'
#' @description Function to calculate a mean connectivity matrix (as a dataframe).
#'
#' @param tbl_conn - dataframe with connectivity information from which to extract means by startZone **x** endZone
#'
#' @return a dataframe with mean, median, and std dev. of connectivity values
#'
#' @details Mean, median, and std deviation of the \code{connFrac} column of the input
#' \code{tbl_conn} are calculated by startZone **x** endZone
#'
#' @import dplyr
#' @import magrittr
#'
#' @export
#'
calcMeanConnectivityMatrix<-function(tbl_conn){
    tmp2 = tbl_conn %>%
              group_by(startZone,endZone) %>%
              summarise(meanFrac=mean(connFrac),sdFrac=stats::sd(connFrac),medFrac=median(connFrac)) %>%
              ungroup() %>%
              mutate(meanPct=100*meanFrac,sdPct=100*sdFrac,medPct=100*medFrac);
    return(tmp2);
}
