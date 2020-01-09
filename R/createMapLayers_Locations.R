#'
#' @title Create map layers from a list of dataframes with point locations
#'
#' @description Function to create map layers from a list of dataframes with point locations.
#'
#' @param dfr_points - dataframe with point locations
#' @param markers - marker type (e.g., "dots", "symmbols")
#' @param shape - integer id for shape type
#' @param size -marker size
#' @param alpha -marker transparency
#' @param basemap - basemap on which to plot locations (if printMaps is TRUE)
#' @param colors.background - basemap background color passed to \code{createBasemap}
#' @param colors.land -basemap land color passed to \code{createBasemap}
#' @param colors.bathym - basemap color for bathymetric contours, passed to \code{createBasemap}
#' @param aes.palette - basemap list for aes.palette passed to \code{createBasemap}
#' @param printMaps - flag to print maps (basemap + each layer)
#'
#' @return list of map layers corresponding to different life stage types
#'
#' @details If basemap is NULL and printMaps is TRUE, a basemap is created
#' using a call to \code{createBasemap} with the supplied parameters.
#'
#' @export
#'
createMapLayers_Locations<-function(
                            dfrs_points,
                            markers=c("dots","symbols"),
                            shape=1,
                            size=0.2,
                            alpha=0.6,
                            basemap=NULL,
                            colors.background="grey85",
                            colors.land="grey45",
                            colors.bathym="grey65",
                            aes.palette=list(cat=c("blue","red"),
                                             div=c("blue","red"),
                                             seq=c("blue","red")),
                            printMaps=FALSE){
  typeNames<-names(dfrs_points);

  #create basemap
  if (printMaps & is.null(basemap)){
    basemap <- createBasemap(colors.background=colors.background,
                             colors.bathym=colors.bathym,
                             colors.land=colors.land,
                             aes.palette=aes.palette)
  }

  #make point-based map layers
  ctr<-1;
  lyrs_points<-list();
  for (typeName in typeNames){
    cat("Creating points map for",typeName,"\n")
    dfr<-dfrs_points[[typeName]];
    lyr<-tmap::tm_shape(dfr,name=typeName);
    if (markers[1]=="dots")    lyr<-lyr+tmap::tm_dots(col="successful",alpha=alpha);
    if (markers[1]=="symbols") lyr<-lyr+tmap::tm_symbols(col="successful",shape=shape,size=size,alpha=alpha,
                                                         border.alpha=0.5*alpha,border.lwd=0.01);
    lyrs_points[[typeName]]<-lyr;
    if (printMaps){
      map <- basemap + lyrs_points[[typeName]];
      print(map);
    }
    ctr<-ctr+1;
  }#--typeName

  return(lyrs_points)
}

# source('createSFDatasets_Points.R')
# dfrsp<-dfrs[[1]][dfrs[[4]]$id<1000,];
# dfrs_points<-createSFDatasets_Points(list(test=dfrsp[dfrsp$age==0.0,]));
# lyrs_points<-createMapLayers_Locations(dfrs_points,markers="symbols",size=0.05,basemap=NULL);

