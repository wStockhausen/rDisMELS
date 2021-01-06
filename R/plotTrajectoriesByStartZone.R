#'
#' @title Plot trajectories from a DisMELS model run by start zone
#'
#' @description Function to plot trajectories from a DisMELS model run by start zone.
#'
#' @param sf_trjs - \pkg{sf} dataframe from \code{\link{indivsInfo_ExtractTrajectories}}
#' @param colorBy - name of column to use as levels to color tracks by
#' @param colorLabel - label for colours legend
#' @param subtitle - plot subtitle
#' @param bmls - basemap layers object (e.g., from \code{\link[rIBMsSnowCrab]{getBasemapLayers}})
#' @param startZones - vector of valid start zones
#' @param nSZpG - number of start zones per map group (1 map is plotted per group)
#' @param nPlotCols - number of columns of maps per page
#' @param colours - vector of colours to use for trajectories
#'
#' @return ggplot2 object
#'
#' @details Separate maps are created for each group of start zones (in column "startZone"),
#' then arranged using \code{\link[ggplot2]{facet_wrap}}. The total number of maps per page
#' will be the number of \code{startZones/nSZpG}, arranged into \code{nPlotCols} columns.
#'
#' The colour values in \code{colours} will be truncated or repeated to match
#' the number of distinct levels in the column specified by \code{colorBy}. It is
#' probably best if this column is a factor, with the expectation that factor levels
#' map to colours by index into the truncated/expanded \code{colours}.
#'
#' @import ggplot2
#' @import magrittr
#' @import sf
#'
#' @export
#'
plotTrajectoriesByStartZone<-function(sf_trjs,
                                      colorBy="successful",
                                      colorLabel=colorBy,
                                      subtitle="",
                                      bmls,
                                      startZones = 1:8,
                                      nSZpG      = 1, #--number of start zones per map group
                                      nPlotCols  = 4, #--number of map columns per page
                                      colours    = c("red","cyan","green","blue","black")
                                      ){
  #----expand colours, as necessary
  ncs = length(unique(sf_trjs[[colorBy]]));
  colours = rep(colours,length.out=ncs);

  #----determine how to group start zones
  nSZs  = length(startZones);  #--number of start zones
  nSZGs = ceiling(nSZs/nSZpG); #--number of start zone groups
  if (nSZGs==nSZs){
    lbls = as.character(startZones);
  } else {
    lbls  = vector(mode="character",length=nSZGs);#--start zone group labels
    for (i in 1:nSZGs) lbls[i] = paste0(startZones[(i-1)*nSZpG+1],":",startZones[min(i*nSZpG,nSZs)]);
  }
  lbls = paste("start zones",lbls);


  #----group trajectories by start zones
  sf_trjs %<>% subset(!is.na(startZone)&(startZone %in% startZones));#drop tracks by individuals starting outside start zones
  sf_trjs$group = factor(floor((sf_trjs$startZone+nSZpG-1)/nSZpG),levels=1:nSZGs,labels=lbls);
  print(sf_trjs$group);
  #sf_trjs$zone_start = factor(sf_trjs$startZone,levels=startZones,labels=paste("zone",startZones));

  #----if necessary, transform to map crs and shift longitudes to 0-360
  if ((sf::st_is_longlat(bmls$map_scale$crs))&(!sf::st_is_longlat(sf_trjs))) {
    crs = bmls$map_scale$crs;
    sf_trjs %<>% sf::st_transform(crs) %>% sf::st_shift_longitude();
  }

  #----plot trajectories by start zone group and colour by colorBy column
  p = ggplot2::ggplot(data=sf_trjs)+bmls$land+
                   ggplot2::geom_sf(mapping=ggplot2::aes_string(colour=colorBy),alpha=0.5)+
                   ggplot2::scale_colour_manual(values=colours)+
                   bmls$zones+bmls$labels+bmls$map_scale+bmls$theme+
                   ggplot2::labs(colour=colorLabel,subtitle=subtitle)+
                   ggplot2::facet_wrap(ggplot2::vars(group),ncol=nPlotCols);
  return(p);
}
