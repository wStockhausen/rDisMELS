#'
#' @title Create a \pkg{ggplot2} map object of a connectivity matrix
#'
#' @description Function to create a \pkg{ggplot2} map object of a connectivity matrix.
#'
#' @param tbl_conn -  tibble with connectivity values to plot
#' @param tbl_web - tibble representing the connectivity web
#' @param bmls - list of base map layers {\pkg{ggplot2} layer objects}
#' @param maxVal - max value for scales (higher values will be set to maxVal)
#' @param valCol - column name for values to plot
#' @param facetGrid - formula for faceting variables using \code{ggplot2::facet_grid}
#' @param facetWrap - column name(s) for faceting variable(s) using \code{ggplot2::facet_wrap}
#' @param facetByRow - flag (T/F) to arrange facets by row
#' @param nrow - number of rows to arrange facets in (if facetByRow is TRUE)
#' @param ncol - number of columns to arrange facets in (if facetByRow is FALSE)
#' @param label - label for legends
#' @param val_min - minimum value to include (0 is never included)
#' @param bin_size - bin size (no binning if NULL)
#' @param bin_max - max bin value, if binning applied
#' @param max_size - max size for connectivity arrows
#' @param linewidth - line width for fixed-width arrows
#' @param alpha - transparency value for fixed-alpha symbols and arrows
#' @param reverse - plot connections in reverse order (largest on bottom)
#' @param scale_colour - \pkg{ggplot2} colour scale for connectivity values (default = ggplot2::scale_colour_viridis_c(option="plasma",limits=c(0,maxVal)))
#' @param scale_size   - \pkg{ggplot2} size scale for connectivity values (default = scale_size_area(max_size=max_size,oob=scales::squish))
#' @param scale_alpha  - \pkg{ggplot2} transparency scale for connectivity values (default = scale_alpha_continuous(oob=scales::squish))
#' @param arrow - pass result from call to \code{\link[grid]{arrow}}
#' @param noAxisLabels - flag (T/F) to include axis labels
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
                             maxVal=NA,
                             valCol="value",
                             facetGrid=NULL,
                             facetWrap=NULL,
                             facetByRow=TRUE,
                             nrow=NULL,
                             ncol=NULL,
                             label="value",
                             val_min=0,
                             bin_size=NULL,
                             bin_max=NULL,
                             max_size=3,
                             linewidth=NA,
                             alpha=NA,
                             reverse=TRUE,
                             scale_colour=ggplot2::scale_colour_viridis_c(option="plasma",limits=c(0,maxVal),oob=scales::squish),
                             scale_size  =ggplot2::scale_size_area(max_size=max_size,oob=scales::squish),
                             scale_alpha =ggplot2::scale_alpha_continuous(oob=scales::squish),
                             arrow=grid::arrow(20,grid::unit(0.15,"inches")),
                             noAxisLabels=TRUE,
                             plotRetentionOnly=FALSE,
                             plotExportedOnly=FALSE){
  #----plot connectivity map
  if (is.na(maxVal)) maxVal = max(tmp[[valCol]],na.rm=TRUE);
  tmp = tbl_conn %>%
          dplyr::inner_join(tbl_web,by=c("startZone","endZone")) %>%
          subset((valCol>0)&(valCol>=val_min));
  if (reverse)  tmp = tmp[rev(order(tmp[[valCol]])),];
  if (!reverse) tmp = tmp[   (order(tmp[[valCol]])),];
  tmp[[".cont_vals"]] = ifelse(tmp[[valCol]]>maxVal,maxVal,tmp[[valCol]]);
  tmp[[".size_vals"]] = tmp[[".cont_vals"]];
  tmp[[".alph_vals"]] = tmp[[".cont_vals"]];
  if (!is.na(alpha)) {
    scale_alpha = scale_alpha_identity();
    tmp[[".alph_vals"]] = alpha;
  }

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
    if (inherits(scale_colour,"ScaleContinuous")){
      message("Changing scale_colour to discrete scale for binned data");
      scale_colour=ggplot2::scale_colour_viridis_d(option="plasma");
    }
  }

   if (is.null(facetGrid)){
    #--use facet_wrap for faceting
    if ((length(facetWrap)==1)&(facetWrap[1]=="startTime")){
      if (facetByRow){
        if (is.null(nrow)) nrow = ceiling(sqrt(length(unique(tmp$startTime))));
        fcts = facet_wrap(vars(format(startTime,format="%Y-%m-%d")),nrow=nrow);
      } else {
        if (is.null(ncol)) ncol = ceiling(sqrt(length(unique(tmp$startTime))));
        fcts = facet_wrap(vars(format(startTime,format="%Y-%m-%d")),ncol=ncol);
      }
    } else {
      if (facetByRow){
        fcts = facet_wrap(facetWrap,nrow=nrow);
      } else {
        fcts = facet_wrap(facetWrap,ncol=ncol);
      }
    }
  } else {
    #--use facet_grid for faceting
    fcts = facet_grid(facetGrid);
  }

  tmp1 = tmp %>% subset(as.character(startZone)!=as.character(endZone));
  # aes1 = aes_string(x="startlon",y="startlat",xend="endlon",yend="endlat",colour=valCol,size=".cont_vals",alpha=".cont_vals");
  tmp2 = tmp %>% subset(as.character(startZone)==as.character(endZone));

  p = ggplot();
  if (!is.null(bmls)) p = p + bmls$land+bmls$zones+bmls$map_scale;
  if ((!plotExportedOnly) & (nrow(tmp2)>0))  {
      aes2 = aes_string(x="startlon",y="startlat",colour=valCol,size=".cont_vals");#--retention values
      p = p + geom_point(data=tmp2,mapping=aes2,inherit.aes=FALSE);
  }
  if ((!plotRetentionOnly)&(nrow(tmp1)>0)) {
    if (is.na(linewidth)){
      aes1 = aes_string(x="startlon",y="startlat",xend="endlon",yend="endlat",colour=valCol,size=".size_vals",alpha=".alph_vals");
      p    = p + geom_segment(data=tmp1,mapping=aes1,arrow=arrow);
    } else {
      aes1 = aes_string(x="startlon",y="startlat",xend="endlon",yend="endlat",colour=valCol,alpha=".alph_vals");
      p    = p + geom_segment(data=tmp1,mapping=aes1,arrow=arrow,size=linewidth);
    }
  }
  p = p + scale_colour + scale_alpha + scale_size + guides(size="none",alpha="none");
  blbls = bmls$labels; blbls$aes_params$colour = "black";
  if (!is.null(bmls)) p = p + blbls;
  p = p + fcts + labs(size=label,colour=label,alpha=label);
  if (!is.null(bmls)) p = p + bmls$theme;
  if (noAxisLabels) p = p + theme(axis.text=element_blank());
  return(p);
}
