#'
#'@title Reorder a set of IBM results by life stage and individual
#'
#'@description Function to reorder a set of IBM results by life stage and individual.
#'
#'@param resFolder - path to folder with results
#'@param resFilePrefix - prefix for file names (default = "Results")
#'@param lifeStageInfo - lhsInfo (list) object with life stage info
#'
#'@return list of data frames by life stage type,
#'with each dataframe ordered by start time, id, and time.
#'
#'@details Requires package \code{readr} to read csv files.
#'
#'@export
#'
reorderResultsByStageAndIndiv<-function(resFolder,
                                        resFilePrefix,
                                        lifeStageInfo){
  info<-lifeStageInfo;
  typeNames<-factor(info$lifeStageTypes$typeName,
                    levels=info$lifeStageTypes$typeName);#typeNames as factor levels

  #read model results by java class and sort by startTime, id and time within a class
  resdr<-resFolder;
  cat("\n\n-----------------\n");
  cat("Reading and sorting results files in",resdr,"\n");
  dfrs<-list();
  for  (cls in names(info$classInfo)){
    cat("\tclass name:",cls,"\n");
    csv<-file.path(resdr,paste0(resFilePrefix,cls,".csv"));
    tmp<-readr::read_csv(csv,skip=1);
    qry<-"select * from tmp
          order by startTime,id,time;"
    tmps<-sqldf::sqldf(qry);
    tmps$typeName<-factor(tmps$typeName,levels=typeNames);#change typeName from character to factor
    dfrs[[cls]]<-tmps;
  }
  rm(csv,tmp,qry,tmps);

  #--reorder model results into list of dataframes by typeName, not java class
  dfrts<-list();
  for (typeName in typeNames){
    dfrt<-NULL;
    for (cls in names(info$classInfo)){
      idx<-dfrs[[cls]]$typeName==typeName;
      if (any(idx)){
        if (sum(idx)==length(dfrs[[cls]]$typeName)) {
          tmp<-dfrs[[cls]];
        } else {
          tmp<-dfrs[[cls]][idx,];
        }
        dfrt<-rbind(dfrt,tmp);
      }
    }
    dfrts[[typeName]]<-dfrt;
  }

  return(dfrts);
}
