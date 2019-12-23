#'
#' @title Classify individuals by success/failure
#'
#' @description Function to classify individuals by success/failure.
#'
#' @param dfrs - list of dataframes by typeName
#' @param index - dataframe returned from \code{extractIndexForSuccessfulIndivs}
#' @param typeName - life stage typeName considered successful (an alternative to providing \code{index})
#'
#' @return the list of dataframes by typeName, with added column "succcess"
#' (TRUE/FALSE) indicating whether individual is considered a success.
#'
#' @details Successful individuals are identified using \code{extractIndexForSuccessfulIndivs(dfrs,typeName)} if
#' \code{index} is null.
#'
#' @export
#'
classifyIndivsBySuccess<-function(dfrs,
                                  index=NULL,
                                  typeName){
  #--get dataframe indicating successful individuals
  idxSuccessfulIndivs<-index;
  if (is.null(idxSuccessfulIndivs))
    idxSuccessfulIndivs<-extractIndexForSuccessfulIndivs(dfrs,typeName);

  #--classify individuals as successful or unsuccessful
  typeNames<-names(dfrs);
  for (typeName in typeNames){
    cat("\n--Identifying successful individuals for",typeName,"\n");
    dfr<-dfrs[[typeName]][,c("origID","startTime")];
    dfr$successful<-FALSE;
    for (r in 1:nrow(idxSuccessfulIndivs)){
      idx<-(dfr$origID==idxSuccessfulIndivs$origID[r])&
           (dfr$startTime==idxSuccessfulIndivs$startTime[r]);
      if (any(idx)) dfr$successful[which(idx)]<-TRUE;
    }
    dfrs[[typeName]]$successful<-dfr$successful;
  }
  return(dfrs);
}
