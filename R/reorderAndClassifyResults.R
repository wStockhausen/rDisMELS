#'
#'@title Reorder a set of IBM results by life stage and individual
#'
#'@description Function to reorder a set of IBM results by life stage and individual.
#'
#' @param resFolder - path to folder with results
#' @param resFilePrefix - prefix for file names (default = "Results")
#' @param lifeStageInfo - lhsInfo (list) object with life stage info
#' @param dfrSuccess - dataframe with individuals classified by success
#' @param crs - coordinate reference system (object that can be converted to an \code{sf::crs} object; default is WGS84)
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#' @param zipFiles - flag to zip DisMELS csv files
#'
#'@return list of \pkg{sf} dataframes by life stage type,
#'with each dataframe ordered by start time, origID, and time. An
#' additional column, 'success', may be present and indicate ultimately
#' successful (TRUE) or unsuccessful (FALSE) individuals. The
#''geom' column contains the point location of each observation.
#'
#' @details Requires package \pkg{readr} to read csv files.
#'
#' @note If locations are in projected coordinates, then \code{wrapDateline}
#' should be set to FALSE (the default is TRUE).
#'
#'@importFrom readr read_csv
#'@importFrom sqldf sqldf
#'@importFrom utils zip
#'@importFrom wtsGIS get_crs
#'
#'@export
#'
reorderAndClassifyResults<-function(resFolder,
                                    resFilePrefix,
                                    lifeStageInfo,
                                    dfrSuccess=NULL,
                                    crs=wtsGIS::get_crs(4326),
                                    wrapDateline=TRUE,
                                    zipFiles=FALSE){
  info<-lifeStageInfo;
  typeNames<-unique(info$lifeStageTypes$typeName);
  typeNames<-factor(typeNames,
                    levels=typeNames);#typeNames as factor levels

  #read model results by java class and sort by startTime, id and time within a class
  resdr<-resFolder;
  cat("\n\n-----------------\n");
  cat("Reading and sorting results files in",resdr,"\n");
  dfrs<-list();
  for  (cls in names(info$classInfo)){
    cat("\tclass name:",cls,"\n");
    csv<-file.path(resdr,paste0(resFilePrefix,cls,".csv"));
    if (file.exists(csv)){
      tmp = readr::read_csv(csv,skip=1);
      if (!is.null(dfrSuccess)){
        cols=names(tmp);
        qry = "select *
               from tmp as t, dfrSuccess as d
               where
                    t.startTime = d.startTime and
                    t.origId   = d.origID
               order by t.startTime,t.id,t.time;"
        tmps = sqldf::sqldf(qry); rm(tmp);
        tmps = tmps[,c(1:length(cols),length(cols)+3)];#drop startTime and origID from Success
        names(tmps) = c(cols,"successful");
      }
      tmps$typeName = factor(tmps$typeName,levels=typeNames);#change typeName from character to factor
      for (typeName in typeNames){
        idx<-tmps$typeName==typeName;
        if (sum(idx)>0){
          cat("\t\ttypeName:",typeName,"\n");
          sf_points = addPointGeometries(tmps[idx,],crs=crs,wrapDateline=wrapDateline);
          dfrs[[typeName]] = sf_points;
          rm(sf_points);
        }
      }
      rm(tmps);
      if (zipFiles) {utils::zip(zipfile=paste0(csv,".zip"),files=csv,flags="-jr9X");}
    }
  }#--cls
  rm(csv,qry);

  return(dfrs);
}
