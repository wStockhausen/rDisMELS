#'
#' @title Create a map layer with values by grid cell
#'
#' @description Function to create a map layer with values by grid cell.
#'
#' @param dfr - dataframe with values to create map layer by gridCellID
#' @param roms_grid - ROMS grid name, shapefile name, or layer (sf dataset) with grid polygons by ID
#' @param join_type - join type for dfr joined to roms_grid
#'
#' @return an \code{sf} (simple features) dataframe with the values by grid cell.
#'
#' @details Uses \code{\link[wtsROMS]{getGrid}} if roms_grid is the name of a ROMS grid.
#' Uses \code{\link[wtsGIS]{createLayerFromShapefile}} if roms_grid is a shapefile.
#' The results depend on the join type:
#'
#'  - right join: matched rows in \code{dfr},         all rows in \code{roms_grid}
#'
#'  - left join:      all rows in \code{dfr},     matched rows in \code{roms_grid}
#'
#'  - inner join: only rows in \code{dfr} matched to \code{roms_grid}
#'
#'  - full join:  all rows in \code{dfr} and all rows in \code{roms_grid}
#'
#' @export
#'
byGridCell_CreateSFDataset<-function(dfr,
                                     roms_grid,
                                     join_type=c("right join","left join","inner join","full join")
                                     ){
  if (is.character(roms_grid)){
    grid<-wtsROMS::getGrid(roms_grid);
    if (is.null(grid)){
      #--create ROMS polygon grid sf dataset
      grid<-wtsGIS::createLayerFromShapefile(roms_grid,
                                             strCRS = wtsGIS::getCRS("AlaskaAlbers"),
                                             as.sf = TRUE);
    }
  }

  dfr_sf<-wtsGIS::mergeDataframeWithLayer(dfr,
                                          grid,
                                          dataID="gridCellID",
                                          geomsID="ID",
                                          sfJoinType=join_type[1],
                                          spDuplicateGeoms=TRUE);
  # #--drop cells without values
  # if (dropEmptyCells){
  #   idx<-!is.na(dfr_sf$xi);
  #   if (any(idx)) {
  #     dfr_sf<-dfr_sf[idx,,drop=FALSE];#keep geometry column
  #   } else return(NULL);#--no non-empty cells
  # }
  return(dfr_sf);
}


