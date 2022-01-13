#'
#'@title Reorder a set of IBM results by life stage, individual and age
#'
#'@description Function to reorder a set of IBM results by life stage, individual and age.
#'
#'@param resFolder - path to folder with results
#'@param resFilePrefix - prefix for file names (default = "Results")
#'@param lifeStageInfo - life stage info (list) object
#'@param startTime - optional value to replace startTimes in model run (default=NULL)
#'
#'@return list of data frames by life stage type,
#'with each dataframe ordered by start time, id, and time.
#'
#'@details Requires package \pkg{readr} to read csv files.
#'
#'@note Times in the csv files are interpreted by readr::read_csv as UTC, as well
#'as in the reordered dataframes.
#'
#'@note Assigning a value to the input \code{startTime} replaces all values of startTime
#'from the model run with the input value in the resulting output. As such, \code{startTime}
#'should only be given when, for example, the individuals in a DisMELS model run were initially released
#'at the same time, but the simulation was restarted with all individuals at the start of a
#'subsequent life stage (resulting in different startTimes for individuals in the DisMELS results for the
#'restarted simulation). Replacing startTime for results from the restarted simulation output yields
#'simulation results equivalent to a continuous model run (i.e., without the restart).
#'
#'
#'@importFrom dplyr arrange
#'@importFrom dplyr bind_rows
#'@importFrom readr read_csv
#'
#'@import magrittr
#'
#'@export
#'
indivsInfo_ReorderResults<-function(resFolder,
                                    resFilePrefix,
                                    lifeStageInfo,
                                    startTime=NULL){
  info<-lifeStageInfo;
  typeNames<-unique(info$lifeStageTypes$typeName);#--don't sort!
  typeNames<-factor(typeNames,
                    levels=typeNames);#typeNames as factor levels

  #read model results by java class and sort by startTime, id and time within a class
  resdr<-resFolder;
  cat("\n\n-----------------\n");
  cat("Reading and sorting results files in",resdr,"\n");
  if (!is.null(startTime)) startTime_ = as.POSIXct(as.character(startTime),tz="UTC");
  dfrs<-list();
  for  (cls in names(info$classInfo)){
    cat("\tclass name:",cls,"\n");
    csv<-file.path(resdr,paste0(resFilePrefix,cls,".csv"));
    if (file.exists(csv)){
      tmp<-readr::read_csv(csv,skip=1);
      tmp$typeName<-factor(tmp$typeName,levels=typeNames);#change typeName from character to factor
      if (is.null(startTime)){
        tmp %<>% dplyr::arrange(startTime,id,typeName,time);
      } else {
        tmp %<>% dplyr::mutate(startTime=startTime_) %>%
                 dplyr::arrange(startTime,id,typeName,time);
      }
      dfrs[[cls]]<-tmp;
      rm(csv,tmp);
      gc(full=TRUE);
    }
  }

  #--reorder model results into list of dataframes by typeName, not java class
  dfrts<-list();
  for (typeName in levels(typeNames)){
    #--for testing: typeName = typeNames[6];
    dfrt=list(); ctr=0;
    for (cls in names(info$classInfo)){
      if (!is.null(dfrs[[cls]])){
        idx<-dfrs[[cls]]$typeName==typeName;
        if (any(idx)){
          if (sum(idx)==length(dfrs[[cls]]$typeName)) {
            tmp<-dfrs[[cls]];
          } else {
            tmp<-dfrs[[cls]][idx,];
          }
          dfrt[[ctr<-ctr+1]]<-tmp;
          rm(tmp);
        }
        rm(idx);
      }
    }#--cls
    if (length(dfrt)>0) {
      if (length(dfrt)==1) {
        dfrts[[typeName]]<-dfrt[[1]];
      } else {
        dfrts[[typeName]]<-dplyr::bind_rows(dfrt);
      }
      rm(dfrt,ctr);
      #gc(full=TRUE,reset=TRUE);
    }
  }#--typeName
  rm(dfrs);
  gc(full=TRUE,reset=TRUE);
  return(dfrts);
}
