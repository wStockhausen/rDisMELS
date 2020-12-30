#'
#' @title Extract IDs for successful individuals
#'
#' @description Function to extract IDs for successful individuals.
#'
#' @param dfrStart - a dataframe or tibble that includes individuals in the initial life stage
#' @param dfrEnd - a dataframe or tibble that includes individuals in the "successful" life stage
#' @param typeNames - vector of names of life stages defining success
#'
#' @return a dataframe or tibble with origID, startTime, and successful for all individuals.
#'
#' @details The
#'
#' @export
#'
indivsInfo_ClassifyBySuccess<-function(dfrStart,
                                       dfrEnd,
                                       typeNames){
  uniqStart=unique(dfrStart[,c("origID","startTime")]);
  idx<-(dfrEnd$ageInStage==0)&(dfrEnd$typeName %in% typeNames);
  uniqEnd<-dfrEnd[idx,c("origID","startTime")];
  qry="select
          s.startTime as startTime,
          s.origID as origID,
          e.origID as test
        from uniqStart as s left join uniqEnd as e
        on s.origID = e.origID and s.startTime = e.startTime
        order by s.startTime, s.origID;";
  dfrp=sqldf::sqldf(qry);
  dfrp$successful = FALSE;
  dfrp$successful[!is.na(dfrp$test)] = TRUE;
  dfrp = dfrp[,c("startTime","origID","successful")];
  return(dfrp);
}
