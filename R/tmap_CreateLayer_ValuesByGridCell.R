#'
#' @title Create a \pkg{tmap}-style map layer with values by grid cell
#'
#' @description Function to create a \pkg{tmap}-style map layer with values by grid cell.
#'
#' @param dfr - \code{sf} dataframe with values to create map layer by gridCellID
#' @param vals_col - column name of values to plot
#' @param basemap - \pkg{tmap}-style basemap to use (default is NULL, which uses the basemap for the CGOA in lat/lon)
#' @param title - map title (default is "values")
#' @param alpha - transparency to apply to values layer (default is 0.9)
#' @param plotMap - flag to combine layer with basemap and plot (default is FALSE)
#'
#' @return a \pkg{tmap}-style layer object of the values by grid cell.
#'
#' @details Requires package \pkg{tmap}. If all values are 0, then NULL is
#' returned for the map layer object. Note that the color scale used for the values
#' is determined by the \code{aes.palette} argument to \code{createBasemap} when the basemap is created.
#'
#' @export
#'
tmap_CreateLayer_ValuesByGridCell<-function(dfr,
                                            vals_col,
                                            basemap=NULL,
                                            title="values",
                                            alpha=0.9,
                                            plotMap=FALSE
                                           ){
  if (!inherits(dfr,"sf")) {
    msg<-"tmap_CreateLayer_ValuesByGridCell: dfr must be an sf dataframe object";
    stop(msg);
  }

  #--drop cells without values
  idx<-!is.na(dfr[[vals_col]]);
  lyr<-NULL;
  if (any(idx)) {
    dfr<-dfr[idx,,drop=FALSE];#keep geometry column

    #--create map with only positive values
    idx<-dfr[[vals_col]]>0;
    if (any(idx)) {
      lyr <- tmap::tm_shape(dfr[idx,,drop=FALSE]) + tmap::tm_fill(vals_col,alpha=alpha);
      if (plotMap){
        if(is.null(basemap)) basemap <- tmap_CreateBasemap();#--use defaults
        map <- basemap + tmap::tm_layout(title=title) + lyr;
        print(map);
      }
    }
  }
  return(lyr);
}


