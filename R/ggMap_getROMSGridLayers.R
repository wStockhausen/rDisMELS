#'
#' @title Create ROMS grid layers for maps based on the \pkg{ggplot2} package
#'
#' @description This function creates a ROMS grid layers for maps based on the \pkg{ggplot2} package.
#'
#' @details The returned list contains land and bathymetry layers (polygons), a fill scale for the bathymetry layer,
#' a map scale, and a partial theme.
#'
#' @param grid - a ROMS grid from call to \code{\link[wtsROMS]{getGrid}}
#' @param final.crs - representation of final (display) coordinate reference system (see details)
#' @param bbox - a bounding box (see details)
#' @param colors.bg - background color
#' @param colors.land - color for land
#' @param scale.bathym - \pkg{ggplot2} scale_fill_ object to use for the bathymetry fill scale
#' @param alpha.bathym - transparency for the bathymetry
#'
#' @return - a list with elements based on the \pkg{ggplot2} package:
#' \itemize{
#' \item{land - geom_sf layer for land-based grid polygons}
#' \item{bathym - geom_sf layer for bathymetry-based grid polygons}
#' \item{fill_scale - scale_fill_ object for bathymetry}
#' \item{map_scale - ggplot2 coord_ object}
#' \item{theme - partial ggplot2 theme with axis titles removed}
#' }
#'
#' @details This convenience function delegates all work to \link[wtsROMS]{gg_CreateROMSGridLayers}.
#'
#' The final coordinate reference system (\code{final.crs}) can be any object that
#' can be converted to a \code{sf::crs} object using \code{\link[wtsGIS]{get_crs}}.
#'
#' The bounding box (\code{bbox}) can be any object that can be converted
#' to a \code{sf::bbox} using \code{\link[wtsGIS]{getBBox}}.
#'
#' @seealso See also \link[wtsROMS]{gg_CreateROMSGridLayers}, \link[wtsGIS]{get_crs},
#' \link[wtsGIS]{getBBox}, \link[wtsGIS]{getStandardBBox}.
#'
#' @import ggplot2
#' @import wtsGIS
#' @import wtsROMS
#'
#' @export
#'
ggMap_getROMSGridLayers = function(grid,
                                   bathym.scale=c(seq(0,200,25),seq(250,500,50),seq(1000,6000,1000)),
                                   final.crs=wtsGIS::get_crs("WGS84"),
                                   bbox=wtsROMS::getStandardBBox("EBS"),
                                   colors.bg="white",
                                   colors.land="grey85",
                                   scale.bathym=ggplot2::scale_fill_viridis_d(option="plasma",direction=-1),
                                   alpha.bathym=1.0){
  res = wtsROMS::gg_CreateROMSGridLayers(grid,
                                         bathym.scale=bathym.scale,
                                         final.crs=final.crs,
                                         bbox=bbox,
                                         colors.bg=colors.bg,
                                         colors.land=colors.land,
                                         scale.bathym=scale.bathym,
                                         alpha.bathym=alpha.bathym);
  return(res);
}
