#'
#' @title Add point geometries to a list dataframes from DisMELS output
#'
#' @description Function to add point geometries to a list dataframes from DisMELS output
#'
#' @param dfrs - list of dataframes, by typeName, or a single dataframe, with DisMELS IBM results
#' @param xCol - name of column with x-axis location
#' @param yCol - name of column with y-axis location
#'
#' @return a list of sf (dataset) objects, by typeName, each with a column ("geom") of class sfc_POINT,
#' or a single sf dataset with a column ("geom") of class sfc_POINT.
#'
#' @details Uses package \code{wtsGIS}. For each sf dataset, the point geometries
#' are in column "geom". Other columns are copied from the associated input dataframes.
#'
#'@importFrom wtsGIS createSF_points
#'
#' @export
#'
addPointGeometries<-function(dfrs,
                             xCol="horizPos1",
                             yCol="horizPos2"){
  if (inherits(dfrs,"data.frame")){
    sf_points<-wtsGIS::createSF_points(dfrs,xCol=xCol,yCol=yCol);
  } else {
    typeNames<-names(dfrs);
    sf_points<-list();
    for (typeName in typeNames){
      cat("Processing",typeName,"\n")
      dfr<-dfrs[[typeName]];
      sf_points[[typeName]]<-wtsGIS::createSF_points(dfr,xCol=xCol,yCol=yCol);
    }
  }
  return(sf_points);
}
