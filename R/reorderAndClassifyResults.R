#'
#'@title Reorder a set of IBM results by life stage and individual
#'
#'@description Function to reorder a set of IBM results by life stage and individual.
#'
#'@param resFolder - path to folder with results
#'@param resFilePrefix - prefix for file names (default = "Results")
#'@param lifeStageInfo - lhsInfo (list) object with life stage info
#'@param dfrSuccess - dataframe with individuals classified by success
#'
#'@return list of \pkg{sf} dataframes by life stage type,
#'with each dataframe ordered by start time, origID, and time. An
#' additional column, 'success', may be present and indicate ultimately
#' successful (TRUE) or unsuccessful (FALSE) individuals. The
#''geom' column contains the point location of each observation.
#'
#'@details Requires package \pkg{readr} to read csv files.
#'
#'@importFrom readr read_csv
#'@importFrom sqldf sqldf
#'
#'@export
#'
reorderAndClassifyResults<-function(resFolder,
                                    resFilePrefix,
                                    lifeStageInfo,
                                    dfrSuccess=NULL){
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
          sf_points = addPointGeometries(tmps[idx,]);
          dfrs[[typeName]] = sf_points;
          rm(sf_points);
        }
      }
      rm(tmps);
    }
  }#--cls
  rm(csv,qry);

  return(dfrs);
}
