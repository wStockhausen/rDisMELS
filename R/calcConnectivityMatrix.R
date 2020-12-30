#'
#' @title Calculate a connectivity matrix dataframe from a connectivity results \pkg{sf} dataframe
#'
#' @description Function to calculate a connectivity matrix datafarme from a connectivity results dataframe.
#'
#' @param sfs_points - list by life stage of \pkg{sf} dataframes with connectivity results and point locations from a single model run
#' @param sf_conn - \pkg{sf} dataframe with connectivity zone information (polygons)
#' @param startStage - name of start life stage for connectivity matrix
#' @param endStage - name of end life stage for connectivity matrix
#' @param endStageFac - multiplier on apparent number successful (e.g., 2 if sexes were split after starting life stage)
#' @param startZones - vector of starting zone id's (integers) for sf_conn
#' @param endZones - vector of ending zone id's (integers) for sf_conn
#' @param plotStartLocs - flag to plot start locations
#' @param plotEndLocs - flag to plot end locations
#' @param bmls - list of \pkg{ggplot2} basemap layers
#' @param crs - \pkg{sf} coordinate reference system (crs) for maps
#' @param nSZpG - number of start zones to include in a group for end location maps
#' @param nPlotCols - number of columns for end location maps
#' @param colours - vector of colours to use to distinguish start zones in an end location map
#'
#' @return list with elements
#' \itemize{
#'   \item{step3_StartEndZones - dataframe with start/end zones for each individual}
#'   \item{step3_ConnectivityDataframe - connectivity matrix as dataframe}
#'   \item{pStartMap - ggplot2 map of start locations (or NULL)}
#'   \item{pEndMap - ggplot2 map of end locations (or NULL)}
#' }
#'
#' @details Calculates the connectivity matrix for a model run based on the connectivity
#' zones defined in \code{sf_conn}. Optionally, \pkg{ggplot2}-style maps of the starting and
#' ending locations of individuals can be created. End zone locations are colored according
#' to their start zone.
#'
#' @import dplyr
#' @import ggplot2
#' @import magrittr
#' @import sf
#' @import tibble
#' @import wtsGIS
#'
#' @export
#'
calcConnectivityMatrix<-function(sfs_points,
                                 sf_conn,
                                 startStage="Z1",
                                 endStage="C1F",
                                 endStageFac=2.0,
                                 startZones=c(1:8),
                                 endZones=c(1:18),
                                 plotStartLocs=FALSE,
                                 plotEndLocs=FALSE,
                                 bmls=NULL,
                                 crs=wtsGIS::get_crs(4326),
                                 nSZpG=2,
                                 nPlotCols=2,
                                 colours=c("red","blue","green","cyan","black")
                              ){
  #--create dataframe for connectivity matrix
  tblSZs = tibble::tibble(startZone=startZones);
  tblEZs = tibble::tibble(endZone  =endZones);
  connZones  = dplyr::full_join(tblSZs,tblEZs,by=character());

  #--extract sf dataframe with individuals at start of simulation
  #--and assign starting connectivity zone
  sf_starts<-sfs_points[[startStage]] %>% subset(startTime==time);
  if (sf::st_crs(sf_conn)!=sf::st_crs(sf_starts)){
    sf_starts<-sf_starts %>% sf::st_transform(sf::st_crs(sf_conn))
  }
  sf_starts %<>% sf::st_join(sf_conn,join=sf::st_within,left=TRUE);
  sf_starts$zone = as.numeric(as.character(sf_starts$zone));
  sf_starts %<>% subset(as.numeric(as.character(zone)) %in% tblSZs$startZone);

  #--extract sf dataframe with individuals at beginning of "end" stage
  #--and assign ending connectivity zone
  sf_ends<-sfs_points[[endStage]] %>% subset(ageInStage==0.0);
  if (sf::st_crs(sf_conn)!=sf::st_crs(sf_ends)){
    sf_ends<-sf_ends %>% sf::st_transform(sf::st_crs(sf_conn))
  }
  sf_ends %<>% sf::st_join(sf_conn,join=sf::st_within,left=TRUE);
  sf_ends$zone = as.numeric(as.character(sf_ends$zone));

  pS = NULL;
  if (plotStartLocs&&!is.null(bmls)){
    #--plot starting locations
    cat("\tprinting start locations\n")
    sf_tmp = sf_starts %>% sf::st_transform(crs) %>% sf::st_shift_longitude();
    pS=ggplot2::ggplot(data=sf_tmp)+bmls$bathym+bmls$land+
                       ggplot2::geom_sf(colour="red",shape=20,alpha=0.5)+
                       #ggplot2::scale_colour_manual(values=rep(c("red","blue"),4))+
                       bmls$zones+bmls$labels+bmls$map_scale+bmls$theme+
                       ggplot2::labs(subtitle="starting locations");
  }

  tbl_starts = sf_starts %>% sf::st_drop_geometry();

  pE=NULL;
  if (plotEndLocs&&!is.null(bmls)){
    #--plot ending locations by start zone
    cat("\tprinting end locations\n")
    sf_tmp = sf_ends %>% sf::st_transform(crs) %>% sf::st_shift_longitude();
    sf_tmp %<>% dplyr::inner_join(tbl_starts[,c("id","origID","startTime","zone")],
                                  by=c("origID"="origID","startTime"="startTime"),
                                  suffix=c("_end","_start"));
    #----determine how to group start zones
    nSZs = length(startZones);#--number of start zones
    nSZGs = ceiling(nSZs/nSZpG); #--number of start zone groups
    lbls = vector(mode="character",length=nSZGs);
    for (i in 1:nSZGs) lbls[i] = paste0((i-1)*nSZpG+1,":",min(i*nSZpG,nSZs));
    lbls = paste("start zones",lbls);
    #----group start zones
    sf_tmp$group = factor(floor((sf_tmp$zone_start+1)/nSZpG),levels=1:nSZGs,labels=lbls);
    sf_tmp$zone_start = factor(sf_tmp$zone_start,levels=1:nSZs);
    pE=ggplot2::ggplot(data=sf_tmp)+bmls$land+
                     ggplot2::geom_sf(mapping=ggplot2::aes(colour=zone_start),shape=20,alpha=0.5)+
                     ggplot2::scale_colour_manual(values=rep(colours[1:nSZpG],nSZGs))+
                     bmls$zones+bmls$labels+bmls$map_scale+bmls$theme+
                     ggplot2::facet_wrap(ggplot2::vars(group),ncol=nPlotCols);
  }

  tbl_ends = sf_ends %>% sf::st_drop_geometry();

  qry1 = "select
           s.startTime,s.origID,e.id,
           s.zone as startZone,s.number as startNum,
           e.zone as endZone,  e.number as endNum
        from tbl_starts as s left join tbl_ends as e
        on s.origID = e.origID and s.startTime = e.startTime
        order by s.startTime, s.origID;";
  t1 = sqldf::sqldf(qry1);
  #wtsUtilities::saveObj(t1,file.path(resFolder,paste0("step3_StartEndZones.RData")));

  qry2a = "select startTime,startZone,sum(startNum) as startNum
          from t1
          group by startTime, startZone;";
  t2a = sqldf::sqldf(qry2a);

  qry2 = "select t1.startTime,s.startZone,sum(t1.startNum) as startNum
          from tblSZs as s left join t1
          on s.startZone=t1.startZone
          group by t1.startTime, s.startZone;";
  t2 = sqldf::sqldf(qry2);

  uStartTimes = tibble::tibble(startTime=unique(t2$startTime));
  uStCZs = dplyr::full_join(uStartTimes,connZones,by=character());

  qry3 = "select u.startTime,u.startZone,u.endZone,sub.endNum
          from uStCZs as u left join
            (select startTime,startZone,endZone,sum(endNum) as endNum
             from t1
             group by startTime,startZone,endZone) as sub
          on  u.startTime=sub.startTime and
              u.startZone=sub.startZone and
              u.endZone=sub.endZone
          order by u.startTime, u.startZone, u.endZone;";
  t3 = sqldf::sqldf(qry3);

  qry4 = "select
            t2.startTime as startTime,
            t2.startZone as startZone,
            t3.endZone as endZone,
            t2.startNum as startNum,
            t3.endNum as endNum
          from t2 left join t3
          on t2.startTime=t3.startTime and t2.startZone=t3.startZone
          order by t2.startTime, t2.startZone, t3.endZone;";
  t4 = sqldf::sqldf(qry4); t4$endNum[is.na(t4$endNum)]=0;
  t4$endNum   = endStageFac * t4$endNum;
  t4 %<>% mutate(connFrac = endNum/startNum);
  #wtsUtilities::saveObj(t4,file.path(resFolder,paste0("step3_ConnectivityDataframe.RData")));

  return(list(step3_StartEndZones=t1,step3_ConnectivityDataframe=t4,pStartMap=pS,pEndMap=pE));
}
