#'
#' @title Create a map layer with values by grid cell
#'
#' @description Function to create a map layer with values by grid cell.
#'
#' @param dfr - dataframe with values to create map layer by gridCellID
#' @param roms_grid - shapefile name or layer (sf dataset) with grid polygons by ID
#'
#' @return an \code{sf} (simple features) dataframe with the values by grid cell.
#'
#' @details Uses package \code{wtsGIS}. Cells in the \code{roms_grid} which are not matched
#' in the dataframe \code{dfr} will have NAs in the corresponding columns from
#' \code{dfr}.
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
                                          duplicateGeoms=TRUE);
  # #--drop cells without values
  # if (dropEmptyCells){
  #   idx<-!is.na(dfr_sf$xi);
  #   if (any(idx)) {
  #     dfr_sf<-dfr_sf[idx,,drop=FALSE];#keep geometry column
  #   } else return(NULL);#--no non-empty cells
  # }
  return(dfr_sf);
}


