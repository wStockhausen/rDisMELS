#'
#' @title Extract IDs for successful individuals
#'
#' @description Function to extract IDs for successful individuals.
#'
#' @param dfr - a dataframe or tibble with the "successful" life stage
#' @param typeName - name of life stage defining success
#'
#' @return a dataframe or tibble with origID, startTime, and successful for all individuals.
#'
#' @details The
#'
#' @export
#'
classifyIndivsBySuccess<-function(dfrStart,
                                  dfrEnd,
                                  typeName){
  uniqStart=unique(dfrStart[,c("origID","startTime")]);
  idx<-(dfrEnd$ageInStage==0)&(dfrEnd$typeName==typeName);
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
