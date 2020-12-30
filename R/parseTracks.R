#'
#' @title Create an sf (simple features) column with linestring geometries from DisMELS tracks
#'
#' @description Function to create an sf (simple features) column with linestring geometries from DisMELS tracks.
#'
#' @param tracks - track column from DisMELS model results file
#' @param crs - coordinate reference system: \pkg{sf} crs object, EPSG code, or character with proj4string
#'
#' @return an object of class sfc_LINESTRING
#'
#' @details Requires packages \code{sf}, \code{wtsGIS}.
#'
#' @import sf
#' @import wtsGIS
#'
#' @export
#'
parseTracks<-function(tracks,
                      crs=wtsGIS::get_crs("WGS84")){
  nt<-length(tracks);
  geoms<-vector(length=nt,mode="list");
  s<-strsplit(tracks,";",fixed=TRUE);
  for (i in 1:nt){
    track<-s[[i]];
    ns<-length(track);
    if (ns>1){
      pos<-matrix(numeric(0),nrow=ns,ncol=3);
      for (j in 1:ns){
        pos[j,]<-as.numeric(strsplit(track[j],":")[[1]]);
      }
      geoms[[i]]<-sf::st_linestring(pos,dim="XYZ");
    } else {
      pos<-as.numeric(strsplit(track[1],":")[[1]]);
      geoms[[i]]<-sf::st_point(pos,dim="XYZ");
    }
  }
  if (nt>1){
    if (sf::st_is(geoms[[1]],c("POINT","POINT Z"))) geoms<-geoms[2:nt];
  }
  sfc_geoms<-sf::st_sfc(geoms,crs=crs);
  sfc_geoms<-sfc_geoms[!sf::st_is_empty(sfc_geoms)];
  return(sfc_geoms);
}


