#'
#' @title Extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries
#'
#' @description Function to extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries.
#'
#' @param sfs_points - list of sf dataframes by typeName (output from \code{\link{indivsInfo_ReorderResults}})
#' @param crs - coordinate reference system for trajectories: \pkg{sf} crs object, EPSG code, or character with proj4string
#' @param step - step size for processing individuals
#' @param verbose - flag to print debugging info
#'
#' @return a \pkg{sf} dataframe with a column ("geom") of class sfc_LINESTRING giving the trajectory of each
#' original individual by life stage (see Details)
#'
#' @details Requires packages \code{sf}, \code{wtsGIS}.
#'
#' @note The output \pkg{sf} dataframe has columns
#' \itemize{
#' \item{typeName}
#' \item{id}
#' \item{parentID}
#' \item{origID}
#' \item{startTime}
#' \item{successful - included if present in sfs_points}
#' \item{additional columns}
#' \item{geom - column with trajectory as an sfg_LINESTRING object}
#' }
#' Additional columns are: max_age, max_ageInStage, max_num, min_num,
#' max_depth, min_depth, mn_depth, max_temp, min_temp, mn_temp.
#'
#' @import dplyr
#' @import sf
#' @import wtsGIS
#'
#' @export
#'
indivsInfo_ExtractTrajectories<-function(sfs_points,
                                         crs=wtsGIS::get_crs("WGS84"),
                                         step=1000,
                                         verbose=FALSE){
  lhss = names(sfs_points);             #--get life stage names
  #--define processing code
  str = "sf_ls = sf_lhsp %>%
                  dplyr::group_by(typeName,id,parentID,origID,startTime &&successful) %>%
                  summarise(max_age=max(age),max_ageInStage=max(ageInStage),
                                  max_num=max(number),min_num=min(number),
                                  max_depth=max(vertPos),min_depth=min(vertPos),mn_depth=mean(vertPos),
                                  max_temp=max(temperature),min_temp=min(temperature),mn_temp=mean(temperature),
                                  do_union=FALSE) %>%
                  sf::st_cast('LINESTRING');"; #--create trajectories
  #--create trajectories
  ctr = 0;
  lst_trjs = list();
  for (lhs in lhss){
    #--for testing: lhs = lhss[1];
    if (verbose) message(paste0("\t\tprocessing ",lhs));
    sf_lhs = sfs_points[[lhs]] %>%
              sf::st_transform(crs);#--transform to Alaska Albers
    if (any(names(sf_lhs)=="successful")){
      strp = gsub("&&successful",",successful",str,fixed=TRUE);
    } else {
      strp = gsub("&&successful","",str,fixed=TRUE);
    }
    dst = sf_lhs %>%
            sf::st_drop_geometry() %>%
            dplyr::distinct(typeName,id,parentID,origID,startTime);
    nd = nrow(dst);
    if (verbose) message(paste0("\t\t\tprocessing ",nd," individuals."));
    min_rws = seq(1,nd,by=step);
    for (min_rw in min_rws){
      #--for testing: min_rw = min_rws[1];
      if (verbose) message(paste0("\t\t\tprocessing individuals ",min_rw," to ",min(min_rw+step-1,nd)));
      dstp = dst[min_rw:min(min_rw+step-1,nd),];
      sf_lhsp = sf_lhs %>% inner_join(dstp);
      eval(parse(text=strp))[[1]];
      lst_trjs[[ctr<-ctr+1]] = sf_ls;
    }
    rm(sf_lhs,sf_ls,strp);
  }
  if (verbose) message(paste0("\t\tbinding rows"));
  sf_trjs = dplyr::bind_rows(lst_trjs);
  return(sf_trjs);
}

