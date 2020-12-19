#'
#'@title Reorder a set of IBM results by life stage and individual
#'
#'@description Function to reorder a set of IBM results by life stage and individual.
#'
#' @param resFolder - path to folder with results
#' @param resFilePrefix - prefix for file names (default = "Results")
#' @param lifeStageInfo - lhsInfo (list) object with life stage info
#' @param flags - string of flags passed to \code{\link[utils]{zip}} (default="-jr9X")
#'
#' @return none.
#'
#' @details Identified DisMELS files are zipped. Originals are not deleted. Using the
#' default \code{flags}, the file path information is stripped from the zipped file.
#' Use "-r9X" to keep the path information.
#'
#' @importFrom utils zip
#'
#' @export
#'
zipDisMELSResults<-function(resFolder,
                            resFilePrefix,
                            lifeStageInfo,
                            flags="-jr9X"){
  info<-lifeStageInfo;
  typeNames<-unique(info$lifeStageTypes$typeName);
  typeNames<-factor(typeNames,
                    levels=typeNames);#typeNames as factor levels

  #read model results by java class and sort by startTime, id and time within a class
  resdr<-resFolder;
  cat("\n\n-----------------\n");
  cat("Zipping DisMELS files in",resdr,"\n");
  dfrs<-list();
  for  (cls in names(info$classInfo)){
    cat("\tclass name:",cls,"\n");
    csv<-file.path(resdr,paste0(resFilePrefix,cls,".csv"));
    cat("\tzipping file '",csv,"'\n",sep="");
    if (file.exists(csv)){
      utils::zip(zipfile=paste0(csv,".zip"),files=csv,flags=flags);
    }
  }
}


