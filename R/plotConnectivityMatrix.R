#'
#' @title Create a \pkg{ggplot2} connectivity matrix plot object
#'
#' @description Function to create a \pkg{ggplot2} connectivity matrix plot object.
#'
#' @param tbl_conn -  tibble with connectivity values to plot
#' @param connPolys - tibble representing the connectivity grid
#' @param valCol - column name for values to plot
#' @param facetCol - column name for faceting variable
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
#' with start zones on the y axis and end zones on the x axis
#'
#' @import ggplot2
#' @import magrittr
#' @importFrom dplyr inner_join
#'
#' @export
#'
plotConnectivityMatrix<-function(tbl_conn,
                                 connPolys,
                                 valCol="value",
                                 facetCol="startTime",
                                 setZeroToNA=FALSE,
                                 xlab="end zone",
                                 ylab="start zone",
                                 fill_lab = "value",
                                 fill_scale=ggplot2::scale_fill_viridis_c(option="plasma"),
                                 legend.position="right"){
  #----plot connectivity matrix
  startZones = sort(unique(connPolys$startZone));
  endZones   = sort(unique(connPolys$endZone));
  tmp = tbl_conn %>%
          dplyr::inner_join(connPolys,by=c("startZone","endZone"));
  if(setZeroToNA) tmp$connPct[tmp[[valCol]]==0] = NA;

  if (facetCol=="startTime"){
    fw = facet_wrap(vars(format(startTime,format="%Y-%m-%d")),nrow=ceiling(sqrt(length(unique(tmp$startTime)))));
  } else {
    fw = facet_wrap(facetCol,nrow=ceiling(sqrt(length(unique(tmp[[facetCol]])))));
  }

  p = ggplot(tmp,mapping=aes_string(x="x",y="y",group="id",fill=valCol)) +
         geom_polygon() + coord_equal(expand=FALSE) +
         fill_scale +
         scale_x_continuous(breaks=endZones) +
         scale_y_continuous(breaks=startZones) +
         fw +
         labs(x=xlab,y=ylab,fill=fill_lab) +
         theme(legend.position=legend.position);

  return(p);
}
