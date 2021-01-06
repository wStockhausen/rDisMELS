#'
#'@title Reorder a set of IBM results by life stage, individual and age
#'
#'@description Function to reorder a set of IBM results by life stage, individual and age.
#'
#'@param resFolder - path to folder with results
#'@param resFilePrefix - prefix for file names (default = "Results")
#'@param lifeStageInfo - life stage info (list) object
#'
#'@return list of data frames by life stage type,
#'with each dataframe ordered by start time, id, and time.
#'
#'@details Requires package \pkg{readr} to read csv files.
#'
#'@note Times in the csv files are interpreted by readr::read_csv as UTC, as well
#'as in the reordered dataframes.
#'
#'@importFrom dplyr arrange
#'@importFrom readr read_csv
#'
#'@export
#'
indivsInfo_ReorderResults<-function(resFolder,
                                    resFilePrefix,
                                    lifeStageInfo){
  info<-lifeStageInfo;
  typeNames<-unique(info$lifeStageTypes$typeName);#--don't sort!
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
      tmp<-readr::read_csv(csv,skip=1);
      tmp$typeName<-factor(tmp$typeName,levels=typeNames);#change typeName from character to factor
      tmps  = tmp %>% dplyr::arrange(startTime,id,typeName,time);
      dfrs[[cls]]<-tmps;
      rm(csv,tmp,tmps);
    }
  }

  #--reorder model results into list of dataframes by typeName, not java class
  dfrts<-list();
  for (typeName in typeNames){
    dfrt<-NULL;
    for (cls in names(info$classInfo)){
      if (!is.null(dfrs[[cls]])){
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
    }#--cls
    if (!is.null(dfrt)) {
      dfrts[[typeName]]<-dfrt;
      rm(dfrt);
    }
  }

  return(dfrts);
}
