#'
#'@title Create a basemap
#'
#' @description Function to create a basemap.
#'
#' @param boundingbox - sf-style bounding box for map (default is for CGOA)
#' @param colors.background - background color
#' @param colors.land - color for land
#' @param colors.bathym - color for bathymetry
#' @param aes.palette - list with color palettes (or vectors) for cat, div, and seq "by" variables
#'
#' @return tmap-style basemap
#'
#' @details Requires packages \code{wtsGIS}, \code{tmap}, and possibly tmaptools.
#' See \code{wtsGIS::getStandardBoundingBox} for alternative, predefined bounding boxes.
#'
#' @export
#'
createBasemap<-function(boundingbox=tmaptools::bb(xlim=c(-162,-134),ylim=c(54,62)),
                        colors.background="grey85",
                        colors.land="grey45",
                        colors.bathym="grey65",
                        aes.palette=list(cat=c("blue","red"),
                                         div=c("blue","red"),
                                         seq=c("blue","red"))){

    basemap <- wtsGIS::createBaseTMap(boundingbox=boundingbox,
                                      colors.bg=colors.background,
                                      colors.land=colors.land,
                                      colors.bathym=colors.bathym) +
                  tmap::tm_layout(aes.palette=aes.palette);
    return(basemap);
}

# basemap<-createBasemap();
# print(basemap);
