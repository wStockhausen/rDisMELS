#'
#' @title Create a map layer with values by grid cell
#'
#' @description Function to create a map layer with values by grid cell.
#'
#' @param dfr - dataframe with values to create map layer by gridCellID
#' @param val_col - column name of values to plot
#' @param roms_grid - shapefile name or layer (sf dataset) with grid polygons by ID
#' @param basemap - basemap to use (default is NULL)
#' @param title - map title (default is "values")
#' @param alpha - transparency to apply to values layer (default is 0.9)
#' @param plotMap - flag to combine layer with basemap and plot (default is FALSE)
#'
#' @return a tmap layer object of the values by grid cell, or NULL if all values are 0.
#'
#' @details Uses packages \code{wtsGIS}, \code{tmap}. If all values are 0, then NULL is
#' returned for the map layer object. Note that the color scale used for the values
#' is determined by the \code{aes.palette} argument to \code{createBasemap} when the basemap is created.
#'
#' @export
#'
createMapLayer_ValuesByGridCell<-function(dfr,
                                          vals_col,
                                          roms_grid,
                                          basemap=NULL,
                                          title="values",
                                          alpha=0.9,
                                          plotMap=FALSE
                                         ){
  if (is.character(roms_grid)){
    #--create ROMS polygon grid sf dataset
    roms_grid<-wtsGIS::createLayerFromShapefile(roms_grid,
                                                strCRS = wtsGIS::getCRS("AlaskaAlbers"),
                                                as.sf = TRUE);
  }

  valsByGC<-wtsGIS::mergeDataframeWithLayer(dfr,
                                            roms_grid,
                                            dataID="gridCellID",
                                            geomsID="ID",
                                            duplicateGeoms=FALSE);
  #--drop cells without values
  idx<-!is.na(valsByGC$xi);
  lyr<-NULL;
  if (any(idx)) {
    valsByGC<-valsByGC[idx,,drop=FALSE];#keep geometry column

    #--create map with only positive values
    idx<-valsByGC[[vals_col]]>0;
    if (any(idx)) {
      lyr <- tmap::tm_shape(valsByGC[idx,,drop=FALSE]) + tmap::tm_fill(vals_col,alpha=alpha);
      if (plotMap){
        if(is.null(basemap)) basemap <- createBasemap();#--use defaults
        map <- basemap + tmap::tm_layout(title=title) + lyr;
        print(map);
      }
    }
  }
  return(lyr);
}


