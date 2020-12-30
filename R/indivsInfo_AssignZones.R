#'
#' @title Assign zones to individuals based on location(s)
#'
#' @description Function to assign zones to individuals based on location(s).
#'
#' @param sf_dfr  - \pkg{sf} dataframe with point geometries
#' @param sf_zones - \pkg{sf} dataframe with zone information (polygons)
#' @param geomNames - vector of names of geometry column(s) to use for assigning zone(s)
#' @param zoneNames - vector of names of zones to be associated with each geomName
#' @param keepFirst - flag to keep first zone in which a point geometry falls (see Details)
#'
#' @return \code{sf_dfr} with additional column(s) indicating the zone(s) in which each geometry falls
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
indivsInfo_AssignZones<-function(sf_dfr,
                                 sf_zones,
                                 geomNames = c("startGeom","endGeom"),
                                 zoneNames = c("startZone","endZone"),
                                 keepFirst=TRUE){
  #--check consistency of keepFirst setting
  inKeepFirst = keepFirst; #--keep input value
  if ((!keepFirst) && (length(geomNames)>1)){
    msg = paste0("rDisMELS::indivsInfo_AssignZones: Warning!\n",
                 "\tAssigning zones to multiple geometry columns, but keepFirst was set to FALSE.\n",
                 "\tResetting keepFirst to TRUE.\n");
    warning(msg);
    keepFirst = TRUE;
  }

  sf_tmp = sf_dfr;#--make a copy (in case crs transformation is required)
  ng = length(geomNames);
  for (ig in 1:ng){
    #--make geomNames[ig] the "active" geometry column
    sf_tmp %<>% sf::st_set_geometry(geomNames[ig]);
    #--if necessary, transform coordinates to crs of sf_zones
    if (sf::st_crs(sf_tmp)!=sf::st_crs(sf_zones)) sf_tmp %<>% sf::st_transform(sf::st_crs(sf_zones));
    #--find zones containing geoms
    zones = (sf_tmp %>%
               sf::st_join(sf_zones,join=sf::st_covered_by,left=TRUE,s2_model="semi_open"));
    #--if necessary, remove multiple-zone matches
    if (keepFirst & (nrow(zones)!=nrow(sf_tmp))){
      #--multiple zones including a point geometry detected, but keeping only first
      if (!inKeepFirst){
        msg = paste0("rDisMELS::indivsInfo_AssignZones: Warning!\n",
                     "\tMultiple zones assigned to at least one geometry in geometry column '",geomNames[ig],"', but only keeping first.\n");
        warning(msg);
      }
      # determine where multiple-zone matches occur
      zones$count = 1;
      res = (zones %>%
              dplyr::group_by(startTime, origID, startLHS,
                              startGridCell, startLon, startLat, startDepth, startBathym, startAge, startNum,
                              endTime, endID, endLHS,
                              endCellID, endLon, endLat, endDepth, endBathym, endAge, endAgeInStage, endNum, successful) %>%
             mutate(tally=cumsum(count)))[["tally"]];#--additional matches marked by tally>1
      zones %<>% subset(res==1);
    }
    #--add zones to sf_dfr
    sf_dfr[[zoneNames[ig]]] = zones$zone;
  }

  return(sf_dfr);
}
#test = indivsInfo_AssignZones(sf_dfr,sf_zones,keepFirst=FALSE);
