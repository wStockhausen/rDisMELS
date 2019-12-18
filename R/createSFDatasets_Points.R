#'
#' @title Create a list of sf (simple features) datasets (dataframes) with point geometries from DisMELS tracks
#'
#' @description Function to create a list of sf (simple features) datasets (dataframes) with point geometries from DisMELS tracks.
#'
#' @param dfrs - list of dataframes, by typeName, with DisMELS IBM results
#' @param strCRS - coordinate reference system: EPSG code or character with proj4string
#'
#' @return a list of sf dataset objects, by typeName, each with a column ("geom") of class sfc_POINT
#'
#' @details Uses packages \code{sf}, \code{wtsGIS}. For each sf dataset, the point geometries
#' are in column "geom". Other columns are copied from the associated input dataframes.
#'
#' @export
#'
createSFDatasets_Points<-function(dfrs){
  typeNames<-names(dfrs);
  dfrs_points<-list();
  for (typeName in typeNames){
    cat("Processing",typeName,"\n")
    dfr<-dfrs[[typeName]];
    dfrs_points[[typeName]]<-wtsGIS::createSFDataset_points(dfr,xCol="horizPos1",yCol="horizPos2");
  }
  return(dfrs_points);
}

# dfrsp<-dfrs[[4]][dfrs[[4]]$id<100,];
# dfrs_points<-createSFDatasets_Points(list(test=dfrsp));
