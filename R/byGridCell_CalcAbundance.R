#'
#' @title Calculate abundance of individuals by grid cell
#'
#' @description Function to calculate abundance of individuals by grid cell.
#'
#' @param dfrs - list of dataframes (or tibbles or sf datasets), by typeName, with DisMELS IBM results
#' @param roms_grid - sf dataset representing a roms grid, with all grid cells of interest
#' @param byStartTime - flag to average by startTime
#' @param verbose - flag to print diagnostic info
#'
#' @return a list of sf datasets, by life stage, with average abundance by grid cell
#'
#' @details Uses packages \code{sqldf}, \code{reshape2}.The number of unique individuals
#' that occupied a grid cell is reported in output dataframe columns ?_indivs. The abundance (averaged
#' over time for each individual, then summed over unique individuals) is reported in
#' output dataframe columns ??_abundance.
#'
#' Calculations can be limited to a subset of grid cells by providing only their IDs
#' (as opposed to all IDs) in \code{roms_grid_IDs}.
#'
#' @import magrittr
#' @import dplyr
#' @import sf
#' @import tibble
#'
#' @export
#'
byGridCell_CalcAbundance<-function(dfrs,
                                   roms_grid,
                                   byStartTime=FALSE,
                                   verbose=FALSE){
  typeNames<-names(dfrs);
  roms_grid %<>% dplyr::select(ID);
  romsInfo<-tibble::tibble(roms_grid_ID=as.character(roms_grid$ID));#--extract grid cell IDs
  lst<-list();
  for (typeName in typeNames){
    #--testing: typeName=typeNames[1];
    cat("\n\nCounting unique individuals by grid cell for",typeName,"\n")
    dfr<-dfrs[[typeName]];
    if (inherits(dfr,"sf")) dfr %<>% sf::st_drop_geometry();
    #--get unique start Times
    uSTs<-tibble::tibble(uniqStartTime=unique(dfr$startTime));
    #--count/sum individuals/abundance by occupied grid cell
    #--obs_count will be the number of times id-with-startTime
    #----is in gridCellID, with classification by success
    #--avg_number will be the averge abundance associated with
    #----that id-with-startTime in gridCellID, with classification by success
    qry1<-"select
             gridCellID,successful,startTime,id,
             count(*) as obs_count,
             sum(number)/count(*) as avg_number
           from dfr
           group by gridCellID,successful,startTime,id
           order by gridCellID,successful,startTime,id;";
    tmp1<-sqldf::sqldf(qry1);
    if (verbose) cat("--Calculated tmp1\n");
    rm(dfr);

    #--aggregate across id (and startTime, possibly)
    #--num_indivs will be the number of unique individuals,
    #----possibly by startTime, in gridCellID,  with classification by success.
    #--avg_abundance will be the total averaged abundance of individuals,
    #----possibly by startTime, in gridCellID, with classification by success
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
    if (verbose) cat("--Calculated tmp2\n");
    rm(tmp1);

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
    if (verbose) cat("--Calculated tmp3's\n");
    rm(tmp2);

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
    tmp3<-dplyr::bind_cols(tmp3a,tmp3b[,c("unsuccessful_abundance","successful_abundance","total_abundance")]);
    if (verbose) cat("--Calculated tmp3\n");
    rm(tmp3a,tmp3b);

    #--expand to all grid cells/startTimes
    if (byStartTime){
      uCs<-sqldf::sqldf("select roms_grid_ID, uniqStartTime from romsInfo,uSTs;");
    } else {
      uCs<-sqldf::sqldf("select roms_grid_ID  from romsInfo;");
    }
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
    if (verbose) cat("--Calculated tmp4\n");
    rm(tmp3);
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

    tmp5 = roms_grid %>% inner_join(tmp4,by=c("ID"="gridCellID"));
    rm(tmp4);
    lst[[typeName]]<-tmp5;
    rm(tmp5);
    if (verbose) cat("--Finished",typeName,"\n");
  }

  return(lst);
}

#indivs<-byGridCell_CalcAbundance(dfrs,roms_grid,byStartTime=FALSE,verbose=TRUE);

