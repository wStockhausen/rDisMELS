#'
#' @title TODO: finish. Create a ggplot2 object with maps from a \pkg{sf} dataframe
#'
#' @description Function to create a ggplot2 object of maps from a \pkg{sf} dataframe.
#'
#' @param sf_dfr - \pkg{sf} dataframe with geometries to plot
#' @param startZones - vector of starting zone id's (integers) for sf_zones
#' @param endZones - vector of ending zone id's (integers) for sf_zones
#' @param plotStartLocs - flag to plot start locations
#' @param plotEndLocs - flag to plot end locations
#' @param bmls - list of \pkg{ggplot2} basemap layers
#' @param crs - \pkg{sf} coordinate reference system (crs) for maps
#' @param nSZpG - number of start zones to include in a group for end location maps
#' @param nPlotCols - number of columns for end location maps
#' @param colours - vector of colours to use to distinguish start zones in an end location map
#'
#' @return a ggplot2 object
#'
#' @details Creates \pkg{ggplot2}-style maps of the starting and
#' ending locations of individuals. End zone locations can be colored according
#' to their start zone.
#'
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @import sf
#' @import tibble
#' @import wtsGIS
#'
#' @export
#'
ggMap_sf<-function(sf_dfr,
                     geomsCol="geometry",
                     colorCol=NULL,
                     colorScale=c("discrete","continuous"),
                     colours=c("red","blue","green","cyan","black"),
                     fillCol=NULL,
                     fillScale=c("discrete","continuous"),
                     fills=c("red","blue","green","cyan","black"),
                     shapeCol=NULL,
                     shapes=c(20,21,22),
                     alpha=0.5,
                     facet_grid=NULL,
                     facet_wrap=NULL,
                     bmls=NULL,
                     crs=wtsGIS::get_crs(4326),
                     nSZpG=2,
                     nPlotCols=2
                  ){
    pE=ggplot2::ggplot(data=sf_tmp)+bmls$land+
                     ggplot2::geom_sf(mapping=ggplot2::aes_string(colour=zone_start),shape=20,alpha=0.5)+
                     ggplot2::scale_colour_manual(values=rep(colours[1:nSZpG],nSZGs))+
                     bmls$zones+bmls$labels+bmls$map_scale+bmls$theme+
                     ggplot2::facet_wrap(ggplot2::vars(group),ncol=nPlotCols);
  return(p);
}
