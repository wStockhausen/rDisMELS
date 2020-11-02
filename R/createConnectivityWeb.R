#'
#' @title Create a \pkg{sf} dataframe with lines between connectivity zones
#'
#' @description Function to create a \pkg{sf} dataframe with lines between connectivity zones
#'
#' @param dfr - a dataframe (or \pkg{sf} dataframe) with lat/lon or xy locations representing connectivity zones
#' @param xCol - name of column with x-axis (longitude) location
#' @param yCol - name of column with y-axis (latitude) location
#' @param crs - coordinate reference system (object that can be converted to an \code{sf::crs} object; default is NA_crs_)
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#'
#' @return a \pkg{sf} dataframe. The 'geometry' column contains lines which connect
#' point locations identifying connectivity zones.
#'
#' @details Creates a \pkg{sf} dataframe with a "web" of directed lines connecting connectivity
#' zones. Coordinates for "start" and "end" zones are also given. If \code{dfr} is an \pkg{sf}
#' dataframe, then the input value for \code{crs} is ignored and taken from \code{dfr}.
#'
#' @import dplyr
#' @import magrittr
#' @import sf
#' @importFrom sfheaders sfg_linestring
#' @importFrom tibble tibble
#'
#' @export
#'
createConnectivityWeb<-function(dfr,
                                xCol="lon",
                                yCol="lat",
                                crs=sf::NA_crs_,
                                wrapDateline=TRUE){
  tmp1 = dfr;
  if (inherits(dfr,"sf")) {
    crs = sf::st_crs(dfr);
    tmp1 = dfr  %>% sf::st_drop_geometry();
  }
  tmp2 = tmp1 %>% dplyr::inner_join(tmp1,by=character(),suffix=c("_start","_end"));
  xStart = paste0(xCol,"_start");
  xEnd   = paste0(xCol,"_end");
  yStart = paste0(yCol,"_start");
  yEnd   = paste0(yCol,"_end");
  nr   = nrow(tmp2);
  geoms = vector(length=nr,mode="list");
  for (rw in 1:nr){
    tbl = tibble::tibble(x=c(tmp2[[xStart]][rw],tmp2[[xEnd]][rw]),
                         y=c(tmp2[[yStart]][rw],tmp2[[yEnd]][rw]));
    geoms[[rw]] = sfheaders::sfg_linestring(tbl,x="x",y="y");
  }
  sfc_geoms = sf::st_sfc(geoms,crs=crs);
  sf_dfr1 = sf::st_sf(dplyr::bind_cols(tmp2,sf::st_sf(geometry=sfc_geoms,crs=crs)));
  xStart = paste0("start",xCol);
  xEnd   = paste0("end", xCol);
  yStart = paste0("start",yCol);
  yEnd   = paste0("end", yCol);
  names(sf_dfr1) = c("startZone",xStart,yStart,"endZone",xEnd,yEnd,"geometry");
  return(sf_dfr1);
}
