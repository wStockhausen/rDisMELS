#'
#' @title Create basemap layers for maps based on the \pkg{ggplot2} package
#'
#' @description This function creates a basemap layer for maps based on the \pkg{ggplot2} package.
#'
#' @details The basemap contains a land layer (polygons) and a bathymetry layer (lines).
#' Uses \code{\link[wtsGIS]{getPackagedLayer}} or \code{\link[wtsUtilities]{readShapefile}} to create the
#' land and bathymetric layers. If \code{bbox} is NULL, then the
#' bounding box for the land layer is used as the bounding box for the basemap.
#'
#' @param layer.land - a \code{sf::sf} object representing land
#' @param layer.bathym - a \code{sf::sf} object representing bathymetry
#' @param final.crs - representation of final (display) coordinate reference system (see details)
#' @param bbox - a bounding box (see details; default is "CGOA shelf")
#' @param colors.bg - background color
#' @param colors.land - color for land
#' @param colors.bathym - color for the bathymetry
#' @param alpha.bathym - transparency for the bathymetry
#'
#' @return - a list of basemap layers based on the \pkg{ggplot2} package, with elements
#' #' \itemize{
#' \item{bathym - geom_sf layer for bathymetry-based grid polygons}
#' \item{land - geom_sf layer for land-based grid polygons}
#' \item{map_scale - ggplot2 coord_sf object}
#' \item{theme - partial ggplot2 theme with axis titles removed}
#' }
#'
#' @details This convenience function delegates all work to \link[wtsGIS]{gg_CreateBasemapLayers}.
#' The final coordinate reference system (\code{final.crs}) can be any object that
#' can be converted to a \code{sf::crs} object using \code{\link[wtsGIS]{get_crs}}.
#'
#' The bounding box (\code{bbox}) can be any object that can be converted
#' to a \code{link[sf]{bbox}} using \code{\link[wtsGIS]{getBBox}}.
#'
#' @seealso See also \link[wtsGIS]{gg_CreateBasemapLayers}, \link[wtsGIS]{getBBox},
#' \link[wtsGIS]{get_crs}, \link[wtsGIS]{getPackagedLayer}, \link[wtsGIS]{getPackagedLayer},
#' \link[wtsROMS]{getStandardBBox}.
#'
#' @import wtsGIS
#' @import wtsROMS
#'
#' @export
#'
ggMap_getBasemapLayers = function(layer.land=wtsGIS::getPackagedLayer("Alaska"),
                                  layer.bathym=wtsGIS::getPackagedLayer("ShelfBathymetry"),
                                  final.crs=wtsGIS::get_crs("WGS84"),
                                  bbox=wtsROMS::getStandardBBox("CGOA shelf"),
                                  colors.bg="white",
                                  colors.land="grey85",
                                  colors.bathym="darkblue",
                                  alpha.bathym=1.0){
  res = wtsGIS::gg_CreateBasemapLayers(layer.land=layer.land,
                                       layer.bathym=layer.bathym,
                                       final.crs=final.crs,
                                       bbox=bbox,
                                       colors.bg=colors.bg,
                                       colors.land=colors.land,
                                       colors.bathym=colors.bathym,
                                       alpha.bathym=alpha.bathym);
  return(res);
}
