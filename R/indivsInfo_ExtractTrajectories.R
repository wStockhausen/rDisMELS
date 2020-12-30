#'
#' @title Extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries
#'
#' @description Function to extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries.
#'
#' @param dfrs - list of dataframes, by typeName, with DisMELS IBM results
#' @param crs - coordinate reference system: \pkg{sf} crs object, EPSG code, or character with proj4string
#'
#' @return a list of \pkg{sf} dataset objects, each with a column ("geom") of class sfc_LINESTRING
#'
#' @details Requires packages \code{sf}, \code{wtsGIS}. For each \pkg{sf} dataframe, the linestring geometry
#' is in column "geom". Other columns include id, startTime, age, ageInStage, and successful.
#'
#' @import sf
#' @import wtsGIS
#'
#' @export
#'
indivsInfo_ExtractTrajectories<-function(dfrs,
                                         crs=wtsGIS::get_crs("WGS84")){
  typeNames<-names(dfrs);
  dfrs_lines<-list();
  for (typeName in typeNames){
    cat("Processing",typeName,"\n")
    start<-Sys.time();
    dfr<-dfrs[[typeName]];
    uIDs<-unique(dfr$id);
    nIDs<-length(uIDs);
    if (nIDs>0){
      # tbl<-NULL;
      tbl<-sf::st_sf(id=integer(nIDs),
                     startTime=character(nIDs),
                     age=numeric(nIDs),
                     ageInStage=numeric(nIDs),
                     successful=logical(nIDs),
                     geom=sf::st_sfc(sf::st_multilinestring(rep(list(matrix(0, 1, 3)),times=nIDs), dim = "XYZ")),
                     row.names=FALSE,
                     crs=crs
                     );
      nID<-0;
      bbox<-NULL;
      for (uID in uIDs){
        nID<-nID+1;
        if (wtsUtilities::mod(nID,100)==0) cat("--Processing",typeName,"track",nID,"of",nIDs,"\n")
        dfrp <-dfr[dfr$id==uID,];
        maxAge<-max(dfrp$ageInStage);
        idx<-which(dfrp$ageInStage==maxAge);
        track<-dfrp$track;
        trajectory<-sf::st_combine(parseTracks(track,crs=crs));#parse tracks, combine into single trajectory
        bbox<-wtsGIS::unionBBox(bbox,sf::st_bbox(trajectory));
        #print(trajectory);
        # tblt<-sf::st_sf(id        =uID,
        #                 startTime =dfrp$startTime[idx],
        #                 age       =dfrp$age[idx],
        #                 ageInStage=dfrp$ageInStage[idx],
        #                 successful=dfrp$successful[idx],
        #                 geom      =trajectory);
        # tbl<-rbind(tbl,tblt);
        tbl$id[nID]        <-uID;
        tbl$startTime[nID] <-dfrp$startTime[idx];
        tbl$age[nID]       <-dfrp$age[idx];
        tbl$ageInStage[nID]<-dfrp$ageInStage[idx];
        tbl$successful[nID]<-dfrp$successful[idx];
        tbl$geom[nID]      <-trajectory;
      }#--uIDs
      attr(sf::st_geometry(tbl), "bbox")<-bbox;
      bbx<-sf::st_bbox(tbl)
      dfrs_lines[[typeName]]<-tbl;
      cat("bbox=",bbox,"\n")
      cat("bbx =",bbx,"\n")
      cat("--elapsed time = ",Sys.time()-start,"\n")
    }#--nIDs>0
  }#--typeNames
  return(dfrs_lines)
}

# dfrsp<-dfrs[[4]][dfrs[[4]]$id<1000,];
# dfrs_trajectories<-createSFDatasets_Trajectories(list(test=dfrsp));
