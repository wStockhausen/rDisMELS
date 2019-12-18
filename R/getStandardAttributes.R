#'
#'@title Get standard attribute names for DisMELS output
#'
#'@description Function to get standard attribute names for DisMELS output from several versions of DisMELS.
#'
#'@param resType - flag indicating DisMELS results type ("Latest","NEW2.0SC","NEW2.0", or "OLD")
#'
#'@return data frame with columns for standard attribute names ('name'), short names ('short_name'), and data types ('data_type')
#'
#'@details Use \code{resType=}\cr
#'  \code{"Latest"} for model results with latest DisMELS version\cr
#'  \code{"NEW2.0SC"} for model results from DisMELS2.0_SnowCrab\cr
#'  \code{"NEW2.0"}   for model results from DisMELS2.0\cr
#'  \code{"OLD"}      for model results from (original) DisMELS\cr
#'
#'@export
#'
getStandardAttributes<-function(resType){
    if (toupper(resType)=='LATEST') resType<-'NEW2.0SC';
    if (toupper(resType)=='NEW2.0SC'){
        dfr<-rbind(data.frame(short_name='typeName',   data_type='character',name="Life stage type name",    stringsAsFactors=FALSE),
                   data.frame(short_name='id',         data_type='integer',  name="ID",                      stringsAsFactors=FALSE),
                   data.frame(short_name='parentID',   data_type='integer',  name="Parent ID",               stringsAsFactors=FALSE),
                   data.frame(short_name='origID',     data_type='integer',  name="Original ID",             stringsAsFactors=FALSE),
                   data.frame(short_name='startTime',  data_type='character',name="Start time (M/D/Y H:M:S)",stringsAsFactors=FALSE),
                   data.frame(short_name='time',       data_type='character',name="Time (M/D/Y H:M:S)",      stringsAsFactors=FALSE),
                   data.frame(short_name='horizType',  data_type='integer',  name="Horiz. position type",    stringsAsFactors=FALSE),
                   data.frame(short_name='vertType',   data_type='integer',  name="Vert. position type",     stringsAsFactors=FALSE),
                   data.frame(short_name='horizPos1',  data_type='numeric',  name="Horiz. position 1",       stringsAsFactors=FALSE),
                   data.frame(short_name='horizPos2',  data_type='numeric',  name="Horiz. position 2",       stringsAsFactors=FALSE),
                   data.frame(short_name='vertPos',    data_type='numeric',  name="Vert. position",          stringsAsFactors=FALSE),
                   data.frame(short_name='bathym',     data_type='numeric',  name="bathymetric depth",       stringsAsFactors=FALSE),
                   data.frame(short_name='gridCellID', data_type='character',name="Grid Cell ID",            stringsAsFactors=FALSE),
                   data.frame(short_name='track',      data_type='character',name="track",                   stringsAsFactors=FALSE),
                   data.frame(short_name='active',     data_type='character',name="Active status",           stringsAsFactors=FALSE),
                   data.frame(short_name='alive',      data_type='character',name="Alive status",            stringsAsFactors=FALSE),
                   data.frame(short_name='age',        data_type='numeric',  name="Age (d)",                 stringsAsFactors=FALSE),
                   data.frame(short_name='ageInStage', data_type='character',name="Age in stage (d)",        stringsAsFactors=FALSE),
                   data.frame(short_name='number',     data_type='numeric', name="Number of individuals",    stringsAsFactors=FALSE));
    } else if (toupper(resType)=='NEW2.0'){
        dfr<-rbind(data.frame(short_name='typeName',   data_type='character',name="Life stage type name",    stringsAsFactors=FALSE),
                   data.frame(short_name='id',         data_type='integer',  name="ID",                      stringsAsFactors=FALSE),
                   data.frame(short_name='parentID',   data_type='integer',  name="Parent ID",               stringsAsFactors=FALSE),
                   data.frame(short_name='origID',     data_type='integer',  name="Original ID",             stringsAsFactors=FALSE),
                   data.frame(short_name='startTime',  data_type='character',name="Start time (M/D/Y H:M:S)",stringsAsFactors=FALSE),
                   data.frame(short_name='time',       data_type='character',name="Time (M/D/Y H:M:S)",      stringsAsFactors=FALSE),
                   data.frame(short_name='horizType',  data_type='integer',  name="Horiz. position type",    stringsAsFactors=FALSE),
                   data.frame(short_name='vertType',   data_type='integer',  name="Vert. position type",     stringsAsFactors=FALSE),
                   data.frame(short_name='horizPos1',  data_type='numeric',  name="Horiz. position 1",       stringsAsFactors=FALSE),
                   data.frame(short_name='horizPos2',  data_type='numeric',  name="Horiz. position 2",       stringsAsFactors=FALSE),
                   data.frame(short_name='vertPos',    data_type='numeric',  name="Vert. position",          stringsAsFactors=FALSE),
                   data.frame(short_name='gridCellID', data_type='character',name="Grid Cell ID",            stringsAsFactors=FALSE),
                   data.frame(short_name='track',      data_type='character',name="track",                   stringsAsFactors=FALSE),
                   data.frame(short_name='active',     data_type='character',name="Active status",           stringsAsFactors=FALSE),
                   data.frame(short_name='alive',      data_type='character',name="Alive status",            stringsAsFactors=FALSE),
                   data.frame(short_name='attached',   data_type='character',name="attached?",               stringsAsFactors=FALSE),
                   data.frame(short_name='age',        data_type='numeric',  name="Age (d)",                 stringsAsFactors=FALSE),
                   data.frame(short_name='ageInStage', data_type='character',name="Age in stage (d)",        stringsAsFactors=FALSE),
                   data.frame(short_name='number',     data_type='numeric', name="Number of individuals",    stringsAsFactors=FALSE));
    } else {
        dfr<-rbind(data.frame(short_name='typeName',   data_type='character',name="Life stage type name",    stringsAsFactors=FALSE),
                   data.frame(short_name='id',         data_type='integer',  name="ID",                      stringsAsFactors=FALSE),
                   data.frame(short_name='parentID',   data_type='integer',  name="Parent ID",               stringsAsFactors=FALSE),
                   data.frame(short_name='origID',     data_type='integer',  name="Original ID",             stringsAsFactors=FALSE),
                   data.frame(short_name='horizType',  data_type='integer',  name="Horiz. position type",    stringsAsFactors=FALSE),
                   data.frame(short_name='vertType',   data_type='integer',  name="Vert. position type",     stringsAsFactors=FALSE),
                   data.frame(short_name='active',     data_type='character',name="Active status",           stringsAsFactors=FALSE),
                   data.frame(short_name='alive',      data_type='character',name="Alive status",            stringsAsFactors=FALSE),
                   data.frame(short_name='attached',   data_type='character',name="attached?",               stringsAsFactors=FALSE),
                   data.frame(short_name='startTime',  data_type='character',name="Start time (M/D/Y H:M:S)",stringsAsFactors=FALSE),
                   data.frame(short_name='time',       data_type='character',name="Time (M/D/Y H:M:S)",      stringsAsFactors=FALSE),
                   data.frame(short_name='age',        data_type='numeric',  name="Age (d)",                 stringsAsFactors=FALSE),
                   data.frame(short_name='ageInStage', data_type='character',name="Age in stage (d)",        stringsAsFactors=FALSE),
                   data.frame(short_name='size',       data_type='numeric',  name="size (mm)",               stringsAsFactors=FALSE),
                   data.frame(short_name='number',     data_type='numeric', name="Number of individuals",    stringsAsFactors=FALSE),
                   data.frame(short_name='horizPos1',  data_type='numeric',  name="Horiz. position 1",       stringsAsFactors=FALSE),
                   data.frame(short_name='horizPos2',  data_type='numeric',  name="Horiz. position 2",       stringsAsFactors=FALSE),
                   data.frame(short_name='temp',       data_type='numeric',  name="temperature (deg C)",     stringsAsFactors=FALSE),
                   data.frame(short_name='salinity',   data_type='numeric',  name="salinity (psu)",          stringsAsFactors=FALSE),
                   data.frame(short_name='vertPos',    data_type='numeric',  name="Vert. position",          stringsAsFactors=FALSE),
                   data.frame(short_name='gridCellID', data_type='character',name="Grid Cell ID",            stringsAsFactors=FALSE),
                   data.frame(short_name='track',      data_type='character',name="track",                   stringsAsFactors=FALSE));
    }
    return(dfr);
}
