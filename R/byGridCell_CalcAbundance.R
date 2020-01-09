#'
#' @title Calculate abundance of individuals by grid cell
#'
#' @description Function to calculate abundance of individuals by grid cell.
#'
#' @param dfrs - list of dataframes, by typeName, with DisMELS IBM results
#' @param roms_grid_IDs - vector (or dataframe with column named roms_grid_ID) with all ids for the roms grid
#' @param byStartTime - flag to average by startTime
#'
#' @return a list of dataframes, by life stage, with average abundance by grid cell
#'
#' @details Uses packages \code{sqldf}, \code{reshape2}.
#'
#' @export
#'
byGridCell_CalcAbundance<-function(dfrs,
                                   roms_grid_IDs,
                                   byStartTime=FALSE){
  typeNames<-names(dfrs);
  romsInfo<-roms_grid_IDs;
  if (is.vector(romsInfo)) {
    #--convert to dtaframe
    romsInfo<-data.frame(roms_grid_ID=roms_grid_IDs,stringsAsFactors=FALSE);
  }
  lst<-list();
  for (typeName in typeNames){
    cat("\n\nCounting unique individuals by grid cell for",typeName,"\n")
    dfr<-dfrs[[typeName]];
    #--get unique start Times
    uSTs<-data.frame(uniqStartTime=unique(dfr$startTime,));
    #--count/sum individuals/abundance by occupied grid cell
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

    #--recast to wide format
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
    rm(tmp3a,tmp3b);

    #--expand to all grid cells/startTimes
    uCs<-sqldf::sqldf("select roms_grid_ID, uniqStartTime from romsInfo,uSTs;");
    qry4<-"select
             roms_grid_ID&&startTime,
             unsuccessful_indivs,   successful_indivs,   total_indivs,
             unsuccessful_abundance,successful_abundance,total_abundance
           from
             uCs as u left join tmp3 as t
           on
             u.roms_grid_ID=t.gridCellID
             &&jStartTime
           order by roms_grid_ID&&startTime;";
    if (byStartTime) {
      str <-",uniqStartTime";
      jstr<-"and u.uniqStartTime=t.startTime";
    } else {str<-jstr<-"";}
    qry4<-gsub("&&startTime", str, qry4,fixed=TRUE);
    qry4<-gsub("&&jStartTime",jstr,qry4,fixed=TRUE);
    tmp4<-sqldf::sqldf(qry4);
    for (col in c("unsuccessful_","successful_","total_")){
      idx<-is.na(tmp4[[paste0(col,"indivs")]]);
      tmp4[[paste0(col,"indivs")]][idx]   <-0;
      tmp4[[paste0(col,"abundance")]][idx]<-0;
    }
    if (byStartTime){
      names(tmp4)[1:2]<-c("gridCellID","startTime");
    } else {
      names(tmp4)[1]<-c("gridCellID");
    }

    lst[[typeName]]<-tmp4;
  }

  return(lst);
}

#indivs<-countIndividualsInGridCells(dfrs,byStartTime=FALSE);

