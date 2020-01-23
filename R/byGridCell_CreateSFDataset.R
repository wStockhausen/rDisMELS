#'
#' @title Create a map layer with values by grid cell
#'
#' @description Function to create a map layer with values by grid cell.
#'
#' @param dfr - dataframe with values to create map layer by gridCellID
#' @param roms_grid - shapefile name or layer (sf dataset) with grid polygons by ID
#'
#' @return an \code{sf} (simple features) dataframe with the values by grid cell, or NULL if all values are 0.
#'
#' @details Uses package \code{wtsGIS}. If all values are 0, then NULL is
#' returned for the sf dataframe object.
#'
#' @export
#'
byGridCell_CreateSFDataset<-function(dfr,
                                     roms_grid
                                     ){
  if (is.character(roms_grid)){
    #--create ROMS polygon grid sf dataset
    roms_grid<-wtsGIS::createLayerFromShapefile(roms_grid,
                                                strCRS = wtsGIS::getCRS("AlaskaAlbers"),
                                                as.sf = TRUE);
  }

  dfr_sf<-wtsGIS::mergeDataframeWithLayer(dfr,
                                          roms_grid,
                                          dataID="gridCellID",
                                          geomsID="ID",
                                          duplicateGeoms=FALSE);
  #--drop cells without values
  idx<-!is.na(dfr_sf$xi);
  if (any(idx)) dfr_sf<-dfr_sf[idx,,drop=FALSE];#keep geometry column
  return(dfr_sf);
}


