#'
#' @title Calculate a set of mean connectivity matrices
#'
#' @description Function to calculate a set of mean connectivity matrices (as a dataframe).
#'
#' @param tbl_conns - dataframe with connectivity information from which to extract stanza means by startZone **x** endZone
#' @param tbl_stanzas - \code{tibble::tibble} with stanza definitions
#'
#' @return a dataframe with mean, median, and std dev. of connectivity values by stanza
#'
#' @details Mean, median, and std deviation of the \code{connFrac} column of the input
#' \code{tbl_conns} are calculated by startZone **x** endZone across each stanza defined in
#' tbl_stanzas.
#'
#' \code{tbl_stanzas} must be a \code{\link[tibble]{tibble}} with columns "stanza" and "years".
#' For each row, \code{stanza} must be a unique label identifying the stanza in question
#' and \code{years} must be a 1-element list containing a vector containing the years over which
#' the stanza is defined.
#'
#' @examples
#' \dontrun{
#' #--assume tbl_conns is the result of rbind'ing several connectivity matrices.
#' }
#'\dontrun{
#' #--a 1-stanza example
#' tbl_stanzas = tibble::tibble(stanza="stanza 1",years=list(2005:2020));
#' tbl_mn = calcMeanConnectivityMatrices(tbl_conns,tbl_stanzas);
#'}
#'
#'\dontrun{
#' #--a 2-stanza example (using \code{\link[tibble]{tribble}} to define stanzas)
#' tbl_stanzas = tibble::tribble(~stanza,    ~years,
#'                               "stanza 1",2005:2020,
#'                               "stanza 2",2015:2030
#'                               );
#' tbl_mn = calcMeanConnectivityMatrices(tbl_conns,tbl_stanzas);
#'}
#'
#' @import dplyr
#' @import magrittr
#'
#' @export
#'
calcMeanConnectivityMatrices<-function(tbl_conns,
                                       tbl_stanzas){
  nr = nrow(tbl_stanzas);
  tbl_mnconns = NULL;
  for (r in 1:nr){
    stanza = tbl_stanzas$stanza[r];
    years  = (tbl_stanzas$years[r])[[1]];
    message(paste("stanza",stanza,"=",paste(years,collapse=", "),"\n"));
    tmp1   = tbl_conns %>% subset(year %in% years);
    tmp2   = calcMeanConnectivityMatrix(tmp1) %>%
               mutate(stanza=stanza,.before=1);
    tbl_mnconns = rbind(tbl_mnconns,tmp2);
  }
  return(tbl_mnconns);
}
