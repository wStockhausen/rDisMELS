#'
#' @title Create a map of values by grid cell
#'
#' @description Function to create a map of values by grid cell.
#'
#' @param dfr - dataframe with values to plot by gridCellID
#' @param val_col - column name of values to plot
#' @param roms_grid - shapefile name or layer (sf dataset) with grid polygons by ID
#' @param basemap - basemap to use
#' @param title - map title
#' @param alpha - transparency to apply to values layer
#'
#' @return a tmap map object of the values plotted by grid cell
#'
#' @details Uses packages \code{wtsGIS}, \code{tmap}. Note that the color scale used for the values
#' is determined by the \code{aes.palette} argument to \code{createBasemap} when the basemap is created.
#'
#' @export
#'
plotMap_ValuesByGridCell<-function(dfr,
                                   vals_col,
                                   roms_grid,
                                   basemap,
                                   title="values",
                                   alpha=0.9
                                   ){
  if (is.character(roms_grid)){
    #--create ROMS polygon grid sf dataset
    roms_grid<-wtsGIS::createLayerFromShapefile(roms_grid,
                                                strCRS = wtsGIS::getCRS("AlaskaAlbers"),
                                                as.sf = TRUE);
  }

  map <- basemap;

  valsByGC<-wtsGIS::mergeDataframeWithLayer(dfr,
                                            roms_grid,
                                            dataID="gridCellID",
                                            geomsID="ID",
                                            duplicateGeoms=FALSE);
  #--drop cells without values
  idx<-!is.na(valsByGC$xi);
  if (any(idx)) {
    valsByGC<-valsByGC[idx,,drop=FALSE];#keep geometry column

    #--create map with only positive values
    idx<-valsByGC[[vals_col]]>0;
    if (any(idx)) {
      map <- map + tmap::tm_layout(title=title) +
               tmap::tm_shape(valsByGC[idx,,drop=FALSE]) + tmap::tm_fill(vals_col,alpha=alpha);
    }
  }
  return(map);
}


