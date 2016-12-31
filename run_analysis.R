
get_data_set <- function(directory="./test",data_set="test"){
    
    
    #before running the analysis
    
    activityLabels="activity_labels.txt"   #Download from oroginal source
    featureFile <-"features.txt"           #Download from orginal source
    
    subjectFile <- paste0("subject_",paste0(data_set,".txt")) #
    measurementsFile <- paste0("x_",paste0(data_set,".txt"))
    activityList <- paste0("y_",paste0(data_set,".txt"))
 
    dirPath=paste0(directory,"/")
 
    #Load Data set components into R - all files have no headers and are space delimited
    dtLabels <-read.table(paste0(dirPath,activityLabels),stringsAsFactors = FALSE)
    dtFeatures <-read.table(paste0(dirPath,featureFile),stringsAsFactors = FALSE)
    dtSubjects <-read.table(paste0(dirPath,subjectFile),stringsAsFactors = FALSE)
    dtActivityList <- read.table(paste0(dirPath,activityList),stringsAsFactors = FALSE)
    dtMeasurements <- read.table(paste0(dirPath,measurementsFile),stringsAsFactors = FALSE)
    
    dtMeasurements <-getCompleteCases(dtMeasurements)
    
    #Replace default variable names with more readable ones
    names(dtLabels) <- c("activity.id","activity.name")
    names(dtFeatures) <-c("feature.id","feature.name")
    names(dtSubjects) <-c("subject.id")
    names(dtActivityList)<-c("activity.id")
    
    #Label each activity with its activity name
    dtLabeledActivities <- merge(x=dtActivityList, y=dtLabels,by=c("activity.id"))
    
    #Cleanup variable names by removing or replacing unsuitable characters from feature names

    dtRenamedFeatures <- rename_features(dtFeatures)
    dtCodeBook <- data.frame(feature_id=dtFeatures$feature.id, original_feature_name=dtFeatures$feature.name, new_feature_name=dtRenamedFeatures$feature.name)
    if(!file.exists("feature_name_transaformation_table.csv")){
       write.csv(dtCodeBook,"feature_name_transformation_table.csv",row.names = FALSE)
    }
    #Replace the default variable names with the feature names
    names(dtMeasurements) <- dtRenamedFeatures$feature.name
    
    #Add subject id's to each observation - row
    dfo<-data.frame(dtSubjects,data.frame(dtLabeledActivities,dtMeasurements))
    #write.csv(dfo,paste0(toupper(data_set),"_CHECK.csv"),row.names=FALSE)
    
    #Label each observation as either TEST or TRAIN
   dfo$data_set <- toupper(data_set)
    dfo
    
}
rename_features <- function(dtFeatures){
    dtFeatures$feature.name <- gsub("-","_",dtFeatures$feature.name) #Replace all dashes(-) with undescores(_)
    dtFeatures$feature.name <- gsub("\\(\\)","",dtFeatures$feature.name) #Remove all ()
    dtFeatures$feature.name <- gsub("\\(","_",dtFeatures$feature.name)  #Replace all remaining '(' with '_'
    dtFeatures$feature.name <- gsub("$\\)","",dtFeatures$feature.name)  #Remove all ')' at the end of a variable name
    dtFeatures$feature.name <- gsub("\\)","",dtFeatures$feature.name)   #Replace all remaining ')' with '_'
    dtFeatures$feature.name <- gsub(",","_",dtFeatures$feature.name)    #Replace all ',' with '_'
    
    dtFeatures
}
getCompleteCases<-function(d){
    #Return a data frame containg only complete observations
    d[complete.cases(d),]
}
run_analysis<-function(){
    #Load the package dplyr if it is not already loaded
    require(dplyr)
    dfTest<-get_data_set() #Default is to use the test data set
    dfTrain<-get_data_set("./train","train")
    # Merge the Training and Test Data sets on all columns
    #   Append any rows that are not matched to the end of the result
    dfCombined <-merge(x=dfTrain, y=dfTest,all=TRUE) 
    
    
    #Extract only MEAN and STANDARD DEVIATIONs of the measurements
    #    and compute their mean 
    #    grouped by Subject Id, Activity ID and Activity Name
    # Either one of Subject.id and Activity.id was adequate for grouping purposes
    # but including both makes it easier to cross check to the source data
    dfc<-grep("_(mean|std)_(X|Y|Z)",names(dfCombined)) 
    dfc <- data.frame(cols=append(c(1,2,3),dfc)) #
    dfcSubset <-dfCombined[dfc$cols]
    
    dfSummary <- dfcSubset %>% group_by(subject.id,activity.id,activity.name) %>% 
        summarise_each(funs(mean),grep("_(mean|std)_(X|Y|Z)",names(dfcSubset)))     
    
    #Export data sets to CSV files
    write.csv(dfcSubset,"Subject_Activity_Mean_Std.csv",row.names = FALSE)
    write.csv(dfSummary,"Subject_Activity_Summary_Mean_Std.csv",row.names = FALSE)
    
    
    #Extract variable names for use in data dictionary = Code Book
    dfVariables <- data.frame(names(dfSummary))
    names(dfVariables) <- c("variable_names")
    write.csv(dfVariables,"variable_names.csv",row.names = FALSE)
    
}