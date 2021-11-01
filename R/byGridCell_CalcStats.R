#'
#' @title Calculate statistics for results by grid cell
#'
#' @description Function to calculate statistics for results by grid cell.
#'
#' @param dfrs - list of dataframes with results by grid cell
#' @param roms_grid - sf dataset representing a roms grid, with all grid cells of interest
#' @param quantity - column name of quantity to calculate statistics for
#' @param lci - lower confidence interval
#' @param uci - upper confidence interval
#' @param byStartTime - flag to average by startTime
#' @param verbose - flag to print diagnostic info
#'
#' @return a list of sf datasets, by life stage, with mean by grid cell
#'
#' @import dplyr
#' @import rlang
#'
#' @export
#'
byGridCell_CalcStats<-function(dfrs,
                               roms_grid,
                               quantity,
                               lci=0.025,
                               uci=0.975,
                               byStartTime=FALSE,
                               verbose=FALSE){
  typeNames<-names(dfrs);
  roms_grid %<>% dplyr::select(ID);
  romsInfo<-tibble::tibble(roms_grid_ID=as.character(roms_grid$ID));#--extract grid cell IDs

  lst<-list();
  for (typeName in typeNames){
    #--testing: typeName=typeNames[1];
    cat("\n\nCalculating stats by grid cell for",typeName,"over unique individuals.\n")
    dfr<-dfrs[[typeName]];
    dfr$id = as.integer(dfr$id);#--convert to explicit integers
    if (quantity %in% names(dfr)){
      if (inherits(dfr,"sf")) dfr %<>% sf::st_drop_geometry();
      #--get unique start Times
      uSTs<-dfr %>% dplyr::distinct(startTime);

      qnt  = rlang::sym(quantity);
      vars = rlang::syms("gridCellID");
      if (byStartTime) vars = rlang::syms(c("startTime","gridCellID"));
      dfr_stats = dfr %>%
                    dplyr::group_by(!!!vars) %>%
                    dplyr::summarize(nobs=dplyr::n(),
                                     nidv=length(unique(id)),
                                     mean=mean(!!qnt,na.rm=TRUE),
                                     stdv=sd(  !!qnt,na.rm=TRUE),
                                     medn=median(!!qnt,na.rm=TRUE),
                                     lci =quantile(!!qnt,probs=lci,na.rm=TRUE,names=FALSE),
                                     uci =quantile(!!qnt,probs=uci,na.rm=TRUE,names=FALSE)) %>%
                    dplyr::ungroup();
      #--convert to sf dataset
      lst[[typeName]] = roms_grid %>% dplyr::inner_join(dfr_stats,by=c("ID"="gridCellID"));
    } else {lst[[typeName]]=NULL;}

    if (verbose) cat("--Finished",typeName,"\n");
  }

  return(lst);
}

