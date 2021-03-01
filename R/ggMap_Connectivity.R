#'
#' @title Create a \pkg{ggplot2} map object of a connectivity matrix
#'
#' @description Function to create a \pkg{ggplot2} map object of a connectivity matrix.
#'
#' @param tbl_conn -  tibble with connectivity values to plot
#' @param tbl_web - tibble representing the connectivity web
#' @param bmls - list of base map layers {\pkg{ggplot2} layer objects}
#' @param valCol - column name for values to plot
#' @param facetCol - column name for faceting variable
#' @param label - label for legends
#' @param val_min - minimum value to include (0 is never included)
#' @param bin_size - bin size (no binning if NULL)
#' @param bin_max - max bin value, if binning applied
#' @param max_size - max size for connectivity arrows
#' @param colour_scale - \pkg{ggplot2} colour scale for connectivity values
#' @param arrow - pass result from call to \code{\link[grid]{arrow}}
#' @param plotRetentionOnly - flag (T/F) to plot only retained proportions
#' @param plotExportedOnly - flag (T/F) to plot only non-retained (exported) proportions
#'
#' @return \pkg{ggplot2} map (plot) object
#'
#' @details Creates a \pkg{ggplot2} map object representing a connectivity matrix
#' as directed arrows from start zone locations to end zone locations.
#'
#'
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom dplyr inner_join
#' @importFrom grid arrow
#' @importFrom grid unit
#'
#' @export
#'
ggMap_Connectivity<-function(tbl_conn,
                              tbl_web,
                              bmls=NULL,
                              valCol="value",
                              facetCol="startTime",
                              label="value",
                              val_min=0,
                              bin_size=NULL,
                              bin_max=NULL,
                              max_size=3,
                              colour_scale=ggplot2::scale_colour_viridis_c(option="plasma"),
                              arrow=grid::arrow(20,grid::unit(0.15,"inches")),
                              plotRetentionOnly=FALSE,
                              plotExportedOnly=FALSE){
  #----plot connectivity map
  tmp = tbl_conn %>% dplyr::inner_join(tbl_web,by=c("startZone","endZone"));
  tmp = tmp[tmp[[valCol]]>0,];
  tmp = tmp[tmp[[valCol]]>=val_min,];
  tmp = tmp[rev(order(tmp[[valCol]])),];
  tmp[[".cont_vals"]] = tmp[[valCol]];

  if (!is.null(bin_size)){
    #--apply binning to values before plotting
    message("Binning connectivity values before plotting.")
    mx = max(tmp[[valCol]]);
    if (is.null(bin_max)) bin_max = mx;
    cutpts = seq(0,bin_max,bin_size);
    tmp[[valCol]] = wtsUtilities::applyCutPts(tmp[[valCol]],c(cutpts,Inf));
    nc = length(cutpts);
    lbls = c(paste0("[",cutpts[1:(nc-1)],",",cutpts[2:(nc)],")"),paste0(cutpts[nc],"+"));
    tmp[[valCol]] = factor(tmp[[valCol]],levels=cutpts,labels=lbls);
    if (inherits(colour_scale,"ScaleContinuous")){
      message("Changing colour_scale to discrete scale for binned data");
      colour_scale=ggplot2::scale_colour_viridis_d(option="plasma");
    }
  }

  if (facetCol=="startTime"){
    fw = facet_wrap(vars(format(startTime,format="%Y-%m-%d")),nrow=ceiling(sqrt(length(unique(tmp$startTime)))));
  } else {
    fw = facet_wrap(facetCol,nrow=ceiling(sqrt(length(unique(tmp[[facetCol]])))));
  }

  tmp1 = tmp %>% subset(startZone!=endZone);
  aes1 = aes_string(x="startlon",y="startlat",xend="endlon",yend="endlat",colour=valCol,size=".cont_vals",alpha=".cont_vals");
  tmp2 = tmp %>% subset(startZone==endZone);
  aes2 = aes_string(x="startlon",y="startlat",colour=valCol,size=".cont_vals");
  if (is.null(bmls)){
    p = ggplot();
    if (!plotRetentionOnly) {if (nrow(tmp1)>0) p = p + geom_segment(data=tmp1,mapping=aes1,arrow=arrow);}
    if (!plotExportedOnly)  {if (nrow(tmp2)>0) p = p + geom_point(data=tmp2,mapping=aes2,inherit.aes=FALSE);}
    p = p + colour_scale + scale_alpha_continuous() + scale_size_area(max_size=max_size);
    p = fw + labs(size=label,colour=label,alpha=label);
  } else {
    p = ggplot() + bmls$land+bmls$zones+bmls$map_scale;
    if (!plotRetentionOnly) {if (nrow(tmp1)>0) p = p + geom_segment(data=tmp1,mapping=aes1,arrow=arrow);}
    if (!plotExportedOnly)  {if (nrow(tmp2)>0) p = p + geom_point(data=tmp2,mapping=aes2,inherit.aes=FALSE);}
    p = p + colour_scale + scale_alpha_continuous() + scale_size_area(max_size=max_size);
    p = p + bmls$labels + fw + labs(size=label,colour=label,alpha=label) + bmls$theme;
  }
  return(p);
}
