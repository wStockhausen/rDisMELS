#'
#' @title Calculate average abundance of individuals by grid cell
#'
#' @description Function to calculate average abundance of individuals by grid cell.
#'
#' @param dfrs - list of dataframes, by typeName, with DisMELS IBM results
#' @param byStartTime - flag to average by startTime
#'
#' @return a list of dataframes, by life stage, with average abundance by grid cell
#'
#' @details Uses packages \code{sqldf}, \code{reshape2}.
#'
#' @export
#'
calcAvgAbundanceByGridCell<-function(dfrs,
                                      byStartTime=FALSE){
  typeNames<-names(dfrs);
  lst<-list();
  for (typeName in typeNames){
    cat("\n\nCounting unique individuals by grid cell for",typeName,"\n")
    dfr<-dfrs[[typeName]];
    qry1<-"select
             gridCellID,successful,startTime,id,
             count(*) as obs_count,
             sum(number)/count(*) as avg_number
           from dfr
           group by gridCellID,successful,startTime,id
           order by gridCellID,successful,startTime,id;";
    tmp1<-sqldf::sqldf(qry1);
    qry2<-"select
             gridCellID,&&startTimesuccessful,
             count(*) as num_indivs,
             sum(avg_number) as avg_abundance
           from tmp1
           group by gridCellID,&&startTimesuccessful
           order by gridCellID,&&startTimesuccessful;";
    if (byStartTime) {str<-"startTime,";} else {str<-"";}
    qry2<-gsub("&&startTime",str,qry2,fixed=TRUE);
    tmp2<-sqldf::sqldf(qry2);
    if (byStartTime){
      tmp3a<-reshape2::dcast(tmp2,gridCellID+startTime~successful,
                            fun.aggregate=wtsUtilities::Sum,value.var="num_indivs");
      tmp3b<-reshape2::dcast(tmp2,gridCellID+startTime~successful,
                            fun.aggregate=wtsUtilities::Sum,value.var="avg_abundance");
      cols<-c("gridCellID","startTime");
    } else {
      tmp3a<-reshape2::dcast(tmp2,gridCellID~successful,
                            fun.aggregate=wtsUtilities::Sum,value.var="num_indivs");
      tmp3b<-reshape2::dcast(tmp2,gridCellID~successful,
                            fun.aggregate=wtsUtilities::Sum,value.var="avg_abundance");
      cols<-"gridCellID";
    }
    #--rename "TRUE", "FALSE" columns as "successful", "unsuccessful"
    if (all(c("TRUE","FALSE") %in% names(tmp3a))){
      names(tmp3a)<-c(cols,"unsuccessful_indivs","successful_indivs");
      tmp3a$total_indivs<-tmp3a$unsuccessful_indivs+tmp3a$successful_indivs;
      names(tmp3b)<-c(cols,"unsuccessful_abundance","successful_abundance");
      tmp3b$total_abundance<-tmp3b$unsuccessful_abundance+tmp3b$successful_abundance;
    } else if ("TRUE" %in% names(tmp3a)){
      names(tmp3a)<-c(cols,"successful_indivs");
      tmp3a$unsuccessful_indivs<-0;
      tmp3a$total_indivs<-tmp3a$unsuccessful_indivs+tmp3a$successful_indivs;
      names(tmp3b)<-c(cols,"successful_abundance");
      tmp3b$unsuccessful_abundance<-0;
      tmp3b$total_abundance<-tmp3b$unsuccessful_abundance+tmp3b$successful_abundance;
    } else if ("FALSE" %in% names(tmp3a)){
      names(tmp3a)<-c(cols,"unsuccessful_indivs");
      tmp3a$successful_indivs<-0;
      tmp3a$total_indivs<-tmp3a$unsuccessful_indivs+tmp3a$successful_indivs;
      names(tmp3b)<-c(cols,"unsuccessful_abundance");
      tmp3b$successful_abundance<-0;
      tmp3b$total_abundance<-tmp3b$unsuccessful_abundance+tmp3b$successful_abundance;
    }
    tmp3<-cbind(tmp3a,tmp3b[,c("unsuccessful_abundance","successful_abundance","total_abundance")]);
    lst[[typeName]]<-tmp3;
  }

  return(lst);
}

#indivs<-countIndividualsInGridCells(dfrs,byStartTime=FALSE);

