#'
#' @title Check connectivity zones for possible problems
#'
#' @description Function to check connectivity zones for possible problems.
#'
#' @param sf_zones - \pkg{sf} dataframe with connectivity zones (polygons)
#'
#' @return list with elements 'validity' and 'overlaps', containing output from running
#' \code{\link[sf]{st_is_valid}} and \code{\link[sf]{st_overlaps}}, respectively,
#' on \code{sf_zones}.
#'
#' @details Ideally, \code{sf_zones} should consist of non-overlapping polygons so that
#' only one zone can be assigned to each geometry. If a point geometry is in an area of overlap
#' between multiple zones, a record would normally be included in the result for each zone
#' in which the point falls. If \code{keepFirst} is TRUE, the record for the first zone
#' in which the point falls is kept. This is also the case if zones are assigned to multiple
#' \code{geomNames}. Otherwise, records are added to the result for all zones in which a point falls.
#'
#' @import magrittr
#' @import sf
#'
#' @export
#'
checkConnectivityZones<-function(sf_zones){
  #--check validity
  vld = sf::st_is_valid(sf_zones,reason=TRUE)
  cat("Validity report:\n")
  for (i in 1:nrow(sf_zones)){
    cat("\tzone ",i,": ",vld[i],"\n",sep="");
  }

  #--check for overlapping polygons
  ovr = sf::st_overlaps(sf_zones,sf_zones);
  cat("Overlap report\n");
  for (i in 1:nrow(sf_zones)){
    if (length(ovr[[i]])>0) cat("\tzone ",i,": ",paste(ovr[[i]],collapse=", "),"\n",sep="");
  }
  return(list(validity=vld,overlaps=ovr));
}
#test = checkConnectivityZones(sf_zones);

