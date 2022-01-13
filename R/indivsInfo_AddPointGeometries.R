#'
#' @title Add point geometries to a list dataframes from DisMELS output
#'
#' @description Function to add point geometries to a list dataframes from DisMELS output
#'
#' @param dfrs - list of dataframes, by typeName, or a single dataframe, with DisMELS IBM results
#' @param xCol - name of column with x-axis location
#' @param yCol - name of column with y-axis location
#' @param crs - coordinate reference system (object that can be converted to an \code{sf::crs} object; default is WGS84)
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#'
#' @return a list of sf (dataset) objects, by typeName, each with a column ("geom") of class sfc_POINT,
#' or a single sf dataset with a column ("geom") of class sfc_POINT.
#'
#' @details Uses package \code{wtsGIS}. For each sf dataset, the point geometries
#' are in column "geom". Other columns are copied from the associated input dataframes.
#'
#' @importFrom wtsGIS get_crs
#' @importFrom wtsGIS createSF_points
#'
#' @export
#'
indivsInfo_AddPointGeometries<-function(dfrs,
                                        xCol="horizPos1",
                                        yCol="horizPos2",
                                        crs=wtsGIS::get_crs(4326),
                                        wrapDateline=TRUE){
  if (inherits(dfrs,"data.frame")){
    sf_points<-wtsGIS::createSF_points(dfrs,
                                       xCol=xCol,
                                       yCol=yCol,
                                       crs=crs,
                                       wrapDateline=wrapDateline);
  } else {
    typeNames<-names(dfrs);
    sf_points<-list();
    for (typeName in typeNames){
      cat("Processing",typeName,"\n")
      sf_points[[typeName]]<-wtsGIS::createSF_points(dfrs[[typeName]],
                                                     xCol=xCol,
                                                     yCol=yCol,
                                                     crs=crs,
                                                     wrapDateline=wrapDateline);
      dfrs[[typeName]]<-NULL;#--not sure if this helps with memory
    }
  }
  rm(dfrs);
  return(sf_points);
}
