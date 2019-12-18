#'
#' @title Update a bbox (bounding box)
#'
#' @description Function to update a bbox (bounding box)
#'
#' @param bbx1 - original bbox
#' @param bbx2 - updating bbox
#'
#' @return resulting bbox
#'
#' @details Returns a bbox covering the union of the individual bboxes.
#'
#' @export
#'
updateBBox<-function(bbx1=NULL,bbx2=NULL){
  if (is.null(bbx1)) return(bbx2);
  if (is.null(bbx2)) return(bbx1);
  bbx<-bbx1;
  bbx["xmin"]<-min(bbx1["xmin"],bbx2["xmin"],na.rm=TRUE);
  bbx["xmax"]<-max(bbx1["xmax"],bbx2["xmax"],na.rm=TRUE);
  bbx["ymin"]<-min(bbx1["ymin"],bbx2["ymin"],na.rm=TRUE);
  bbx["ymax"]<-max(bbx1["ymax"],bbx2["ymax"],na.rm=TRUE);
  return(bbx);
}
