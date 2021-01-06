#'
#' @title Extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries
#'
#' @description Function to extract trajectories from DisMELS output as a list of \pkg{sf} dataframes with linestring geometries.
#'
#' @param sfs_points - list of sf dataframes by typeName (output from \code{\link{indivsInfo_ReorderResults}})
#' @param crs - coordinate reference system for trajectories: \pkg{sf} crs object, EPSG code, or character with proj4string
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
#' \item{successful}
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
                                         crs=wtsGIS::get_crs("WGS84")){
  lhss = names(sfs_points);             #--get life stage names
  #--create trajectories
  sf_trjs = NULL;
  for (lhs in lhss){
    cat("\t\tprocessing",lhs,"\n");
    sf_lhs = sfs_points[[lhs]] %>%
              sf::st_transform(crs);#--transform to Alaska Albers
    sf_ls = sf_lhs %>%
              dplyr::group_by(typeName,id,parentID,origID,startTime,successful) %>%
              dplyr::summarize(max_age=max(age),max_ageInStage=max(ageInStage),
                              max_num=max(number),min_num=min(number),
                              max_depth=max(vertPos),min_depth=min(vertPos),mn_depth=mean(vertPos),
                              max_temp=max(temperature),min_temp=min(temperature),mn_temp=mean(temperature),
                              do_union=FALSE) %>%
              sf::st_cast("LINESTRING");#--create trajectories
    sf_trjs = rbind(sf_trjs,sf_ls);
    rm(sf_lhs,sf_ls);
  }
  return(sf_trjs);
}

