######################################################################
# XPRESS - OHDSI Edition                                             #
# Beta release                                                       #
# Available at: http://www.github.com/jmbanda/OHDSI/Phenotyping/     #
# getPatientData.R                                                   #
# Requires: R and Java 1.6 or higher #                               #
######################################################################
# Install necessary packages if needed, remove comments
#install.packages("devtools")   
#install_github("ohdsi/SqlRender")
#install_github("ohdsi/DatabaseConnector")

# Load libraries
library(devtools)
library(SqlRender)
library(DatabaseConnector)
library(plyr)

###########################################################
# All parameters connection info and extras are in        #
# the file config.R, please make any appropiate changes   #
###########################################################
folder = "/home/jmbanda/Phenotyping/public_beta/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

source("config.R")
###########################################################
# End of parameters. Make no changes after this #
###########################################################

connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)

conn <- connect(connectionDetails)
#Get the cases and controls
keywordList_FF <- read.table('keywordlist.tsv', sep="\t", header=FALSE)
ignoreList_FF <- read.table('ignorelist.tsv', sep="\t", header=FALSE)

#Get all the concept ids of interest
concept_ids_to_query <- as.character(keywordList_FF$V3)
concept_ids_to_ignore <- as.character(ignoreList_FF$V3)

patients_list_df<- list()

getPatientswithconceptMentionsSql <- paste("SELECT distinct(person_id) FROM @cdmSchema.observation WHERE observation_concept_id IN (",paste(concept_ids_to_query,collapse=","),",",paste(concept_ids_to_ignore,collapse=","),") AND qualifier_concept_id=0;",sep='')
renderedSql <- renderSql(getPatientswithconceptMentionsSql, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
if (dbms=="postgresql") {  
  patients_list_df[[1]] <- dbGetQueryPostgreSql(conn,translatedSql)
} else {
  patients_list_df[[1]] <- dbGetQuery.ffdf(conn,translatedSql)  
}

getPatientswithconceptMentionsSql <- paste("SELECT distinct(person_id) FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(concept_ids_to_query,collapse=","),",",paste(concept_ids_to_ignore,collapse=","),");",sep='')
renderedSql <- renderSql(getPatientswithconceptMentionsSql, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
if (dbms=="postgresql") {  
  patients_list_df[[2]] <- dbGetQueryPostgreSql(conn,translatedSql)
} else {
  patients_list_df[[2]] <- dbGetQuery.ffdf(conn,translatedSql)  
}

patientsALL_df <- do.call(rbind, patients_list_df)
remove('patients_list_df')

if (nCases > nrow(patientsALL_df)) {
  stop("Not enough patients to get the number of cases specified")
} 
cases<-patientsALL_df[sample(nrow(patientsALL_df), nCases),]
write.table(cases, file=paste('cases.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)

potentialControlsSQL <- paste("SELECT person_id FROM @cdmSchema.person WHERE person_id NOT IN (",paste(as.character(patientsALL_df$person_id),collapse=","),");" ,sep='')
renderedSql <- renderSql(potentialControlsSQL, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
if (dbms=="postgresql") {  
    potentialControls_df <- dbGetQueryPostgreSql(conn,translatedSql)
} else {
  potentialControls_df <- dbGetQuery.ffdf(conn,translatedSql)  
}
if (nControls > nrow(patientsALL_df)) {
  stop("Not enough patients to get the number of controls specified")
} 
controls<-potentialControls_df[sample(nrow(potentialControls_df), nControls),]
write.table(controls, file=paste('controls.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
remove('potentialControls_df')


cases_pids <- read.table('cases.tsv', sep="\t", header=FALSE)
controls_pids <- read.table('controls.tsv', sep="\t", header=FALSE)

keywords <- as.character(keywordList_FF$V3)
ignores <- as.character(ignoreList_FF$V3)
rm('keywordList_FF', 'ignoreList_FF')

if (flag_drugexposures) {
  patientFeatures_cases_drugexposures_df<- list()
}
if (flag_observations) {
  patientFeatures_cases_observations_df<- list()
}
if (flag_visits) {
  patientFeatures_cases_visits_df<- list()
}
if (flag_labs) {
  patientFeatures_cases_labs_df<- list()
}

for (patientQueue in 1:nCases) {
      patients_list_df<- list()
      getPatientswithconceptMentionsSql <- paste("SELECT observation_concept_id, observation_date FROM @cdmSchema.observation WHERE observation_concept_id IN (",paste(keywords,collapse=","),",",paste(ignores,collapse=","),") AND qualifier_concept_id=0 AND person_id=",as.character(cases_pids[patientQueue,1]),";",sep='')
      renderedSql <- renderSql(getPatientswithconceptMentionsSql, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
      translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
      if (dbms=="postgresql") {  
        patients_list_df[[1]] <- dbGetQueryPostgreSql(conn,translatedSql)
      } else {
        patients_list_df[[1]] <- dbGetQuery.ffdf(conn,translatedSql)  
      }
      
      ### Using the condition_occurrence table for the term mentions
      getPatientswithconceptMentionsSql <- paste("SELECT condition_concept_id, condition_start_date AS observation_date FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(keywords,collapse=","),",",paste(ignores,collapse=","),") AND person_id=",as.character(cases_pids[patientQueue,1]),";",sep='')
      renderedSql <- renderSql(getPatientswithconceptMentionsSql, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
      translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
      if (dbms=="postgresql") {  
        patients_list_df[[2]] <- dbGetQueryPostgreSql(conn,translatedSql)
      } else {
        patients_list_df[[2]] <- dbGetQuery.ffdf(conn,translatedSql)  
      }
      dates <- do.call(rbind, patients_list_df)
      remove('patients_list_df')
      
      dateFrom<-min(dates$observation_date)

      if (flag_drugexposures) {
          getPatientPrescriptions <- paste("SELECT drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_type_concept_id, stop_reason FROM @cdmSchema.drug_exposure WHERE person_id=",as.character(cases_pids[patientQueue,1])," AND drug_exposure_start_date >='",as.character(dateFrom),"';",sep='')
          renderedSql <- renderSql(getPatientPrescriptions, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
          translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
          if (dbms=="postgresql") {  
             tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
          } else {
            tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
          }
          

          if (nrow(tmp_fv) >0) { #deal with patients with no entries
            test1<-aggregate( drug_exposure_id ~ drug_concept_id, tmp_fv, function(x) length(unique(x)))
            names(test1)[names(test1)=="drug_concept_id"] <- "concept_id"
            names(test1)[names(test1)=="drug_exposure_id"] <- "counts"
            test1<-data.frame(t(test1))
            colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
            test1<-test1[-c(1), , drop=FALSE]
          } else {
            test1 <- data.frame(t(data.frame(x = numeric(0))))
          }
          row.names(test1)<-as.character(cases_pids[patientQueue,1])   
          patientFeatures_cases_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
          rm('test1')         
          rm('tmp_fv')
          
          
          
      }
      if (flag_observations) {
          getPatientObservations <- paste("SELECT observation_id, person_id, observation_concept_id, observation_date, observation_type_concept_id FROM @cdmSchema.observation WHERE person_id=",as.character(cases_pids[patientQueue,1])," AND observation_date>='", as.character(dateFrom),  "' AND qualifier_concept_id = 0;",sep='')
          renderedSql <- renderSql(getPatientObservations, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
          translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
          if (dbms=="postgresql") {  
            tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
          } else {
            tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
          }
          

          if (nrow(tmp_fv) >0) { #deal with patients with no entries    
            test1<-aggregate( observation_id ~ observation_concept_id, tmp_fv, function(x) length(unique(x)))
            names(test1)[names(test1)=="observation_concept_id"] <- "concept_id"
            names(test1)[names(test1)=="observation_id"] <- "counts"
            #change colums to rows
            test1<-data.frame(t(test1))
            colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
            test1<-test1[-c(1), , drop=FALSE]
          } else {
            #create an empty dataset 
            test1 <- data.frame(t(data.frame(x = numeric(0))))    
          }
          row.names(test1)<-as.character(cases_pids[patientQueue,1])  
          patientFeatures_cases_observations_df[[patientQueue]]<-test1
          rm('test1')          
          rm('tmp_fv')          
          
      }
      if (flag_visits) {
          getPatientVisits <- paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id FROM @cdmSchema.visit_occurrence as A, ohdsiv5.condition_occurrence as B WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.visit_start_date >='",as.character(dateFrom), "' AND A.person_id=",as.character(cases_pids[patientQueue,1]),";",sep='')
          renderedSql <- renderSql(getPatientVisits, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
          translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
          if (dbms=="postgresql") {  
            tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
          } else {
            tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
          }
          
          
          if (nrow(tmp_fv) >0) { #deal with patients with no entries  
            test1<-aggregate( condition_occurrence_id ~ condition_concept_id, tmp_fv, function(x) length(unique(x)))
            names(test1)[names(test1)=="condition_concept_id"] <- "concept_id"
            names(test1)[names(test1)=="condition_occurrence_id"] <- "counts"
            #change colums to rows
            test1<-data.frame(t(test1))
            colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
            test1<-test1[-c(1), , drop=FALSE]
          } else {
            #create an empty dataset 
            test1 <- data.frame(t(data.frame(x = numeric(0))))
          }  
          row.names(test1)<-as.character(cases_pids[patientQueue,1])  
          patientFeatures_cases_visits_df[[patientQueue]]<-test1
          rm('test1')          
          rm('tmp_fv')    
          
          
      }
      if (flag_labs)  {
          getPatientLabs <- paste("SELECT measurement_id, person_id, measurement_date, measurement_type_concept_id, value_as_number, value_as_concept_id FROM @cdmSchema.measurement WHERE person_id=",as.character(cases_pids[patientQueue,1])," AND measurement_date >='",as.character(dateFrom),"';",sep='')
          renderedSql <- renderSql(getPatientLabs, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
          translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
          if (dbms=="postgresql") {  
            tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
          } else {
            tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
          }

          if (nrow(tmp_fv) >0) { #deal with patients with no entries  
            #Remove lab values that did not map properly
            tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="0"),]
            #Remove lab values that have a measurement of NONE
            tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="4124462"),]
            #Create unique categorical categories for lab values
            tmp_fv$type_valueM <- paste(tmp_fv$measurement_type_concept_id, tmp_fv$value_as_concept_id, sep=":") 
            test1<-aggregate( measurement_id ~ type_valueM, tmp_fv, function(x) length(unique(x)))
            
            test1<-data.frame(t(test1))
            colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
            test1<-test1[-c(1), , drop=FALSE]            
          } else {
            #create an empty dataset 
            test1 <- data.frame(t(data.frame(x = numeric(0))))           
          }
          row.names(test1)<-as.character(cases_pids[patientQueue,1])  
          patientFeatures_cases_labs_df[[patientQueue]]<-test1
          rm('test1')          
          rm('tmp_fv')                
      }
}

if (saveALLresults) {
  if (flag_observations) {
    save(patientFeatures_cases_drugexposures_df,file=paste(studyName,"-RAW_FV_CASES_drugexposures_",as.character(nCases),".Rda",sep=''))
  }
  if (flag_visits) {
    save(patientFeatures_cases_visits_df,file=paste(studyName,"-RAW_FV_CASES_visits_",as.character(nCases),".Rda",sep=''))
  }
  if (flag_observations) {
    save(patientFeatures_cases_observations_df,file=paste(studyName,"-RAW_FV_CASES_observations_",as.character(nCases),".Rda",sep=''))
  }
  
  if (flag_labs) {
    save(patientFeatures_cases_labs_df,file=paste(studyName,"-RAW_FV_CASES_labs_",as.character(nCases),".Rda",sep=''))
  }
}

######################################## CONTROLS ##############################################################################################

if (flag_drugexposures) {
  patientFeatures_controls_drugexposures_df<- list()
}
if (flag_observations) {
  patientFeatures_controls_observations_df<- list()
}
if (flag_visits) {
  patientFeatures_controls_visits_df<- list()
}
if (flag_labs) {
  patientFeatures_controls_labs_df<- list()
}

for (patientQueue in 1:nControls) {
  #Here we need to grab all data from each patient based on pidsFromFile  
  if (flag_drugexposures) {
    getPatientPrescriptions <- paste("SELECT drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_type_concept_id, stop_reason FROM @cdmSchema.drug_exposure WHERE person_id=",as.character(controls_pids[patientQueue,1]),";",sep='')
    renderedSql <- renderSql(getPatientPrescriptions, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
    translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
    if (dbms=="postgresql") {  
      tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
    } else {
      tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
    }
    
    
    if (nrow(tmp_fv) >0) { #deal with patients with no entries
      test1<-aggregate( drug_exposure_id ~ drug_concept_id, tmp_fv, function(x) length(unique(x)))
      names(test1)[names(test1)=="drug_concept_id"] <- "concept_id"
      names(test1)[names(test1)=="drug_exposure_id"] <- "counts"
      test1<-data.frame(t(test1))
      colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
      test1<-test1[-c(1), , drop=FALSE]
    } else {
      #create an empty dataset for empty patients so we don't end up with non-existing empty vectors
      test1 <- data.frame(t(data.frame(x = numeric(0))))
    }
    row.names(test1)<-as.character(controls_pids[patientQueue,1])   
    patientFeatures_controls_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
    rm('test1')         
    rm('tmp_fv')
  }
  if (flag_observations) {
    getPatientObservations <- paste("SELECT observation_id, person_id, observation_concept_id, observation_date, observation_type_concept_id FROM @cdmSchema.observation WHERE person_id=",as.character(controls_pids[patientQueue,1])," AND qualifier_concept_id = 0;",sep='')
    renderedSql <- renderSql(getPatientObservations, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
    translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
    if (dbms=="postgresql") {  
      tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
    } else {
      tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
    }
    
    
    if (nrow(tmp_fv) >0) { #deal with patients with no entries    
      test1<-aggregate( observation_id ~ observation_concept_id, tmp_fv, function(x) length(unique(x)))
      names(test1)[names(test1)=="observation_concept_id"] <- "concept_id"
      names(test1)[names(test1)=="observation_id"] <- "counts"
      #change colums to rows
      test1<-data.frame(t(test1))
      colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
      test1<-test1[-c(1), , drop=FALSE]
    } else {
      #create an empty dataset 
      test1 <- data.frame(t(data.frame(x = numeric(0))))    
    }
    row.names(test1)<-as.character(controls_pids[patientQueue,1])  
    patientFeatures_controls_observations_df[[patientQueue]]<-test1
    rm('test1')          
    rm('tmp_fv')          
    
  }
  if (flag_visits) {
    getPatientVisits <- paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id FROM @cdmSchema.visit_occurrence as A, ohdsiv5.condition_occurrence as B WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.person_id=",as.character(controls_pids[patientQueue,1]),";",sep='')
    renderedSql <- renderSql(getPatientVisits, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
    translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
    if (dbms=="postgresql") {  
      tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
    } else {
      tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
    }
    
    
    if (nrow(tmp_fv) >0) { #deal with patients with no entries  
      test1<-aggregate( condition_occurrence_id ~ condition_concept_id, tmp_fv, function(x) length(unique(x)))
      names(test1)[names(test1)=="condition_concept_id"] <- "concept_id"
      names(test1)[names(test1)=="condition_occurrence_id"] <- "counts"
      #change colums to rows
      test1<-data.frame(t(test1))
      colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
      test1<-test1[-c(1), , drop=FALSE]
    } else {
      #create an empty dataset 
      test1 <- data.frame(t(data.frame(x = numeric(0))))
    }  
    row.names(test1)<-as.character(controls_pids[patientQueue,1])  
    patientFeatures_controls_visits_df[[patientQueue]]<-test1
    rm('test1')          
    rm('tmp_fv')    
  }
  if (flag_labs)  {
    getPatientLabs <- paste("SELECT measurement_id, person_id, measurement_date, measurement_type_concept_id, value_as_number, value_as_concept_id FROM @cdmSchema.measurement WHERE person_id=",as.character(controls_pids[patientQueue,1]),";",sep='')
    renderedSql <- renderSql(getPatientLabs, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
    translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
    if (dbms=="postgresql") {  
      tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
    } else {
      tmp_fv <- dbGetQuery.ffdf(conn,translatedSql)  
    }
    
    if (nrow(tmp_fv) >0) { #deal with patients with no entries  
      #Remove lab values that did not map properly
      tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="0"),]
      #Remove lab values that have a measurement of NONE
      tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="4124462"),]
      #Create unique categorical categories for lab values
      tmp_fv$type_valueM <- paste(tmp_fv$measurement_type_concept_id, tmp_fv$value_as_concept_id, sep=":") 
      test1<-aggregate( measurement_id ~ type_valueM, tmp_fv, function(x) length(unique(x)))
      
      test1<-data.frame(t(test1))
      colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
      test1<-test1[-c(1), , drop=FALSE]            
    } else {
      #create an empty dataset 
      test1 <- data.frame(t(data.frame(x = numeric(0))))           
    }
    row.names(test1)<-as.character(cases_pids[patientQueue,1])  
    patientFeatures_controls_labs_df[[patientQueue]]<-test1
    rm('test1')          
    rm('tmp_fv')         
  }
}



if (saveALLresults) {
    if (flag_observations) {
      save(patientFeatures_controls_drugexposures_df,file=paste(studyName,"-RAW_FV_CONTROLS_drugexposures_",as.character(nControls),".Rda",sep=''))
    }
    if (flag_visits) {
      save(patientFeatures_controls_visits_df,file=paste(studyName,"-RAW_FV_CONTROLS_visits_",as.character(nControls),".Rda",sep=''))
    }
    if (flag_observations) {
      save(patientFeatures_controls_observations_df,file=paste(studyName,"-RAW_FV_CONTROLS_observations_",as.character(nControls),".Rda",sep=''))
    }
    
    if (flag_labs) {
      save(patientFeatures_controls_labs_df,file=paste(studyName,"-RAW_FV_CONTROLS_labs_",as.character(nControls),".Rda",sep=''))
    }
}


### Merge Feature vectors

#Get the Patients FV for Observations
if (flag_observations) {
  test<-append(patientFeatures_cases_observations_df, patientFeatures_controls_observations_df)
  FV_ob <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_ob <- ddply(FV_ob, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
  colnames(FV_ob)<-paste('obs:',colnames(FV_ob),sep='')
  colnames(FV_ob)[1]<-"pid"
  rm(test)
}

#Get the Patients FV for Visits
if (flag_visits) {
  test<-append(patientFeatures_cases_visits_df, patientFeatures_controls_visits_df)
  FV_v <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_v <- ddply(FV_v, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
  colnames(FV_v)<-paste('visit:',colnames(FV_v),sep='')
  colnames(FV_v)[1]<-"pid"
  rm(test)
}
#Get the Patients FV for drug exposures
if (flag_drugexposures) {
  test<-append(patientFeatures_cases_drugexposures_df, patientFeatures_controls_drugexposures_df)
  FV_de <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_de <- ddply(FV_de, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
  colnames(FV_de)<-paste('drugexp:',colnames(FV_de),sep='')
  colnames(FV_de)[1]<-"pid"
  rm(test)
}

patientFeatures_controls_labs_df_n<- list()
patientFeatures_cases_labs_df_n<- list()

for (patientQueue in 1:nCases) {
  if (ncol(patientFeatures_cases_labs_df[[patientQueue]])==0) {
    patientFeatures_cases_labs_df_n[[patientQueue]]<-patientFeatures_cases_labs_df[[patientQueue]]
  } else {
    tmpJP<-patientFeatures_cases_labs_df[[patientQueue]]
    i<- sapply(tmpJP, is.factor)
    tmpJP[i] <- lapply(tmpJP[i], as.character)
    tmp_col <- colnames(tmpJP)
    tmpJP <- as.data.frame(lapply(tmpJP, as.numeric))
    colnames(tmpJP) <- tmp_col
    row.names(tmpJP)<-as.character(cases_pids[patientQueue,1]) 
    patientFeatures_cases_labs_df_n[[patientQueue]]<-tmpJP
  }
}
for (patientQueue in 1:nControls) {
  if (ncol(patientFeatures_controls_labs_df[[patientQueue]])==0) {
    patientFeatures_controls_labs_df_n[[patientQueue]]<-patientFeatures_controls_labs_df[[patientQueue]]
  } else {
    tmpJP<-patientFeatures_controls_labs_df[[patientQueue]]
    i<- sapply(tmpJP, is.factor)
    tmpJP[i] <- lapply(tmpJP[i], as.character)
    tmp_col <- colnames(tmpJP)
    tmpJP <- as.data.frame(lapply(tmpJP, as.numeric))
    colnames(tmpJP) <- tmp_col
    row.names(tmpJP)<-as.character(controls_pids[patientQueue,1]) 
    patientFeatures_controls_labs_df_n[[patientQueue]]<-tmpJP
  }
}

if (flag_labs) {
  test<-append(patientFeatures_cases_labs_df_n, patientFeatures_controls_labs_df_n)
  FV_lab <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_lab[is.na(FV_lab)]<-0
  colnames(FV_lab)<-paste('lab:',colnames(FV_lab),sep='')
  colnames(FV_lab)[1]<-"pid"
  rm(test)
}

#We now have the FULL feature vectors to send to the classifier

if (flag_drugexposures) {
  save(FV_de,file=paste(studyName,"-FULL_FV_drugexposures_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
}
if (flag_visits) {
  save(FV_v,file=paste(studyName,"-FULL_FV_visits_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
}
if (flag_observations) {
  save(FV_ob,file=paste(studyName,"-FULL_FV_observations_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
}
if (flag_labs) {
  save(FV_lab,file=paste(studyName,"-FULL_FV_labs_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
}





