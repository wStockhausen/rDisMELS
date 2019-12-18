##--------------------DEPRECATED 2019-12-17-------------------------------------
#'
#'@title Extract trajectories of (selected) individuals from a DisMELS model run
#'
#'@description Function to extract trajectories of (selected) individuals from a DisMELS model run.
#'
#'@param results - DisMELS results file (or dataframe)
#'@param indivIDs - vector of id's for individuals to extract (NULL = all)
#'@param lsTypesInfo - life stage types info list for IBM
#'@param lsTypes - vector of names of life stage types to extract (NULL = all)
#'@param returnList - flag (T/F) indicating whether a list should be returned (otherwise NULL--helps with memory)
#'@param writeOutput - flag (T/F) indicating whether output files should be written
#'@param outDir - folder for output
#'@param outBaseCSV - base name for output csv files
#'@param verbose - flag (T/F) to print debugging info
#'
#'@return list of dataframes with individual results by lhs type name if returnList=TRUE, otherwise NULL.
#'
#'@details If results is NULL, the user will be prompted to select the
#'corresponding file using a file chooser dialog.
#'Uses functions \code{wtsUtilities::getCSV} and \code{sqldf::sqldf}.
#'
#'@export
#'
extractTrajectories<-function(results=NULL,    #DisMELS results file (or dataframe)
                              indivIDs=NULL,
                              lsTypesInfo=NULL,
                              lsTypes=NULL,
                              returnList=TRUE,
                              writeOutput=TRUE,
                              outDir='./',
                              outBaseCSV="allIndivs",
                              verbose=FALSE
                                ){

    retRes<-FALSE;
    if (!is.data.frame(results)){
        #read in full results of DisMELS model run from csv file
        if (is.null(results)){
            results<-wtsUtilities::getCSV(caption='Select full DisMELS results file');
            if (is.null(results)) {
                return(NULL);#user aborted
            }
        } else {
            cat("Reading results from '",results,"'\n",sep='');
            results<-read.csv(results,stringsAsFactors=FALSE);
        }
        retRes<-TRUE;
    }

    #define life stage types
    if (is.null(lsTypes)) lsTypes<-names(lsTypesInfo$lifeStageTypes);#process all typeNames

    #define variables
    stdVars <-getStdVars(lhsTypesInfo$resType)$vars;
    stdVarsOut<-c('typeName','id','time',
                  'horizPos1','horizPos2','vertPos','bathymetric_depth','track',
                  'alive','age','ageInStage','number');

    #pull out IDs for individuals, if not provided
    if (is.null(indivIDs)) indivIDs <- unique(results$id);
    cat('Will extract results for ',nrow(indivIDs),' individuals\n',sep='')
    qry<-"select distinct ID from indivIDs order by ID;"
    uids<-sqldf::sqldf(qry);
    cat("Number of unique ids in indivIDs =",nrow(uids),'\n');
    print(uids$ID[1:10]);

    #loop over type names and extract results for indivs
    ctr<-0;
    if (returnList) indivsLst<-list();
    for (lsType in lsTypes){
      ctr<-ctr+1;
      cat("Extracting results for '",lsType,"'\n",sep='');
      #extract indivs from results
      lsTI<-lsTypesInfo[[lsType]];
      resVars<-paste('r',names(lsTI$info$vars),sep='.',collapse=',');
      qry<-"select
              $$stdVars
              &&resVars
            from
              results r
            where
              r.id=i.ID and
              r.typeName='&&typeName'
            order by
              r.id,r.age;";
      qry<-gsub("&&resVars",resVars,qry);
      qry<-gsub("&&typeName",typeName,qry);
      cat("query = ",qry,sep='\n');
      indivsTmp<-sqldf::sqldf(qry);

      #check on indivs
      qry<-"select distinct id from indivsTmp order by id;"
      uids<-sqldf::sqldf(qry);
      cat("Number of unique ids in indivsTmp =",nrow(uids),'\n');
      print(uids$id[1:10]);
      cat('names(indivsTmp)= [',paste(names(indivsTmp),collapse=','),']\n')

      #assign correct names to columns for life stage
      lhsVars<-lhsTypeInfo$lifeStageTypes[[typeName]]$info$vars;
      allVars<-c(conVars,stdVars$vars,lhsVars);
      names(indivsTmp)<-paste('tmp',1:ncol(indivsTmp),sep='');#assign temporary names
      nms<-names(indivsTmp);
      nms[1:length(allVars)]<-allVars; #substitute allVars
      names(indivsTmp)<-nms;           #assign names to extracted columns

      #determine output column names for life stage
      varsOutStr<-paste(allVars,sep='',collapse=',')

      qry<-"select
              &&varsOut
            from
              indivsTmp
            order by
              id,age;";
      qry<-gsub("&&varsOut",varsOutStr,qry);
      cat("query = ",qry,sep='\n');
      indivsType<-sqldf::sqldf(qry);

      #write indivs to csv file
      if (writeOutput) write.csv(indivsType,file=file.path(outDir,paste(outBaseCSV,ctr,lsType,'csv',sep='.')),row.names=FALSE);
      if (returnList) indivsLst[[lsType]]<-indivsType;
    }#loop over lsTypes

    if (returnList) return(invisible(indivsLst));
    return(NULL);
}

#icDir<-'~/Programming/R/GitPackages/wtsDisMELSConn'
#indivConn<-file.path(icDir,'IndivConn.1997.csv')
#resDir<-'/Volumes/Iomega HDD/cDrive.GOA_IERP/IBM_Runs/ATF/FullSeriesJanFeb/CSVs.ModelResults'
#results<-file.path(resDir,'Results1997.csv')
#sIndivs<-extractIndivs(indivConn,results,onlySuccessful  =TRUE,lhsTypes='egg01',outBaseCSV='ExtractedIndivs.Successful',  lhsTypeInfo=getLifeStageInfo.ATF(),returnList=FALSE);
#uIndivs<-extractIndivs(indivConn,results,onlyUnsuccessful=TRUE,lhsTypes='egg01',outBaseCSV='ExtractedIndivs.Unsuccessful',lhsTypeInfo=getLifeStageInfo.ATF(),returnList=FALSE);
