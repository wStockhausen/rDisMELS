#'
#' @title Create a ggplot2 object with individual locations from a \pkg{sf} dataframe
#'
#' @description Function to create a ggplot2 object with individual locations from a \pkg{sf} dataframe.
#'
#' @param sf_dfr - \pkg{sf} dataframe with geometries to plot
#' @param colorBy - name of column to use as levels to color tracks by
#' @param colorLabel - label for colours legend
#' @param startZones - vector of starting zone id's (integers) for sf_zones
#' @param subtitle - plot subtitle
#' @param bmls - list of \pkg{ggplot2} basemap layers
#' @param  group - flag to group startZones
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
#' @import ggplot2
#' @import magrittr
#' @import sf
#'
#' @export
#'
plotMap_IndivLocations<-function(sf_dfr,
                                 geomCol="geometry",
                                 colorBy="successful",
                                 colorLabel=colorBy,
                                 startZones=c(1:8),
                                 subtitle="",
                                 group=FALSE,
                                 bmls=NULL,
                                 nSZpG=1,
                                 nPlotCols=2,
                                 colours=c("red","blue","green","cyan","black")
                              ){
  #----set "active" geometry column
  sf_dfr %<>% sf::st_set_geometry(geomCol);

  #----expand or truncate colours, as necessary
  if (!is.null(colorBy)){
    ncs = length(unique(sf_dfr[[colorBy]]));
    colours = rep(colours,length.out=ncs);
  }

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

  #----group individual locations by start zones
  sf_dfr %<>% subset(!is.na(startZone)&(startZone %in% startZones));#drop individuals starting outside start zones
  sf_dfr$group = factor(floor((sf_dfr$startZone+nSZpG-1)/nSZpG),levels=1:nSZGs,labels=lbls);

  #----if necessary, transform to map crs and shift longitudes to 0-360
  crs = bmls$map_scale$crs;
  if ((sf::st_is_longlat(crs))&(!sf::st_is_longlat(sf_dfr))) {
    sf_dfr %<>% sf::st_transform(crs) %>% sf::st_shift_longitude();
  }

  p=ggplot2::ggplot(data=sf_dfr)+bmls$land;
  if ( is.null(colorBy)) p = p + ggplot2::geom_sf(colour=colours[1],shape=20,alpha=0.5);
  if (!is.null(colorBy)) p = p + ggplot2::geom_sf(mapping=ggplot2::aes_string(colour=colorBy),shape=20,alpha=0.5);
  p = p + ggplot2::scale_colour_manual(values=colours)+
          bmls$zones+bmls$labels+bmls$map_scale+bmls$theme+
          ggplot2::labs(colour=colorLabel,subtitle=subtitle);
  if (group) p = p + ggplot2::facet_wrap(ggplot2::vars(group),ncol=nPlotCols);

  return(p);
}


