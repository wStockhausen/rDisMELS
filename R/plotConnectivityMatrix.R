#'
#' @title Create a \pkg{ggplot2} connectivity matrix plot object
#'
#' @description Function to create a \pkg{ggplot2} connectivity matrix plot object.
#'
#' @param tbl_conn -  tibble with connectivity values to plot
#' @param connPolys - tibble representing the connectivity grid
#' @param maxVal - max value for fill scale
#' @param valCol - column name for values to plot
#' @param facetGrid - formula for faceting variables using \code{ggplot2::facet_grid}
#' @param facetWrap - column name(s) for faceting variable(s) using \code{ggplot2::facet_wrap}
#' @param facetByRow - flag (T/F) to arrange wrapped facets by row
#' @param nrow - number of rows to arrange wrapped facets in (if facetByRow is TRUE)
#' @param ncol - number of columns to arrange wrapped facets in (if facetByRow is FALSE)
#' @param setZeroToNA - flag (T/F) to set 0 values to NA's
#' @param xlab - x axis label
#' @param ylab - x axis label
#' @param fill_lab - label for fill legend
#' @param fill_scale - \pkg{ggplot2} fill scale
#' @param legend.position - string indicating legend position in \code{\link[ggplot2]{theme}}
#'
#' @return \pkg{ggplot2} plot object
#'
#' @details Creates a \pkg{ggplot2} plot object representing a connectivity matrix
#' with start zones on the y axis and end zones on the x axis. The startZone and endZone
#' columns of the connPolys dataframe should be factors, with levels corresponding to
#' the order in which zones should be displayed.
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom dplyr inner_join
#'
#' @export
#'
plotConnectivityMatrix<-function(tbl_conn,
                                 connPolys,
                                 maxVal=NA,
                                 valCol="value",
                                 facetGrid=NULL,
                                 facetWrap=NULL,
                                 facetByRow=TRUE,
                                 nrow=NULL,
                                 ncol=NULL,
                                 setZeroToNA=FALSE,
                                 xlab="end zone",
                                 ylab="start zone",
                                 fill_lab = "value",
                                 fill_scale=ggplot2::scale_fill_viridis_c(option="plasma",limits=c(0,maxVal)),
                                 legend.position="right"){
  #----plot connectivity matrix
  startZones = levels(connPolys$startZone);
  endZones   = levels(connPolys$endZone);
  if (!is.factor(tbl_conn$startZone)) tbl_conn$startZone = factor(tbl_conn$startZone,levels=startZones,labels=startZones);
  if (!is.factor(tbl_conn$endZone))   tbl_conn$endZone   = factor(tbl_conn$endZone,  levels=endZones,  labels=endZones);
  tmp = tbl_conn %>%
          dplyr::inner_join(connPolys,by=c("startZone","endZone"));
  if(setZeroToNA) tmp[[valCol]][tmp[[valCol]]==0] = NA;

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

  p = ggplot(tmp,mapping=aes_string(x="x",y="y",group="id",fill=valCol)) +
         geom_polygon() + coord_equal(expand=FALSE) +
         geom_abline(slope=1,linetype=2) +
         fill_scale +
         scale_x_continuous(breaks=1:length(endZones),  labels=endZones) +
         scale_y_continuous(breaks=1:length(startZones),labels=startZones) +
         fcts +
         labs(x=xlab,y=ylab,fill=fill_lab) +
         theme(legend.position=legend.position);

  return(p);
}
