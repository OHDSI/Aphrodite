######################################################################
# XPRESS - OHDSI Edition                                             #
# Beta release                                                       #
# Available at: http://www.github.com/jmbanda/OHDSI/Phenotyping/     #
# predict.R                                                       #
# Requires: R and Java 1.6 or higher #                               #
######################################################################
# Install necessary packages if needed, remove comments
#install.packages("devtools")   
#install_github("ohdsi/SqlRender")
#install_github("ohdsi/DatabaseConnector")
#install.packages("caret")
#install.packages("pROC")

# Load libraries
library(SqlRender)
library(DatabaseConnector)
library(plyr)
library(caret)
library(pROC)



connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)

conn <- connect(connectionDetails)
cases_pids <- read.table(toPredict, sep="\t", header=FALSE)
nCases=nrow(cases_pids)

#### REMOVE ##################
local=1
#### REMOVE ##################

if (local==0) {
      if (flag_drugexposures) {
        patientFeatures_cases_p_drugexposures_df<- list()
      }
      if (flag_observations) {
        patientFeatures_cases_p_observations_df<- list()
      }
      if (flag_visits) {
        patientFeatures_cases_p_visits_df<- list()
      }
      if (flag_labs) {
        patientFeatures_cases_p_labs_df<- list()
      }
      
      for (patientQueue in 1:nCases) {
          if (flag_drugexposures) {
            getPatientPrescriptions <- paste("SELECT drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_type_concept_id, stop_reason FROM @cdmSchema.drug_exposure WHERE person_id=",as.character(cases_pids[patientQueue,1]),";",sep='')
            renderedSql <- renderSql(getPatientPrescriptions, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
            translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
            tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
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
            patientFeatures_cases_p_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
            rm('test1')         
            rm('tmp_fv')
          }
          if (flag_observations) {
            getPatientObservations <- paste("SELECT observation_id, person_id, observation_concept_id, observation_date, observation_type_concept_id FROM @cdmSchema.observation WHERE person_id=",as.character(cases_pids[patientQueue,1])," AND qualifier_concept_id = 0;",sep='')
            renderedSql <- renderSql(getPatientObservations, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
            translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
            tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
            if (nrow(tmp_fv) >0) { #deal with patients with no entries    
              test1<-aggregate( observation_id ~ observation_concept_id, tmp_fv, function(x) length(unique(x)))
              names(test1)[names(test1)=="observation_concept_id"] <- "concept_id"
              names(test1)[names(test1)=="observation_id"] <- "counts"
              test1<-data.frame(t(test1))
              colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
              test1<-test1[-c(1), , drop=FALSE]
            } else {
              test1 <- data.frame(t(data.frame(x = numeric(0))))    
            }
            row.names(test1)<-as.character(cases_pids[patientQueue,1])  
            patientFeatures_cases_p_observations_df[[patientQueue]]<-test1
            rm('test1')          
            rm('tmp_fv')          
          }
          if (flag_visits) {
            getPatientVisits <- paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id FROM @cdmSchema.visit_occurrence as A, ohdsiv5.condition_occurrence as B WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.person_id=",as.character(cases_pids[patientQueue,1]),";",sep='')
            renderedSql <- renderSql(getPatientVisits, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
            translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
            tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
            if (nrow(tmp_fv) >0) { #deal with patients with no entries  
              test1<-aggregate( condition_occurrence_id ~ condition_concept_id, tmp_fv, function(x) length(unique(x)))
              names(test1)[names(test1)=="condition_concept_id"] <- "concept_id"
              names(test1)[names(test1)=="condition_occurrence_id"] <- "counts"
              test1<-data.frame(t(test1))
              colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
              test1<-test1[-c(1), , drop=FALSE]
            } else {
              test1 <- data.frame(t(data.frame(x = numeric(0))))
            }  
            row.names(test1)<-as.character(cases_pids[patientQueue,1])  
            patientFeatures_cases_p_visits_df[[patientQueue]]<-test1
            rm('test1')          
            rm('tmp_fv')    
          }
          if (flag_labs)  {
            getPatientLabs <- paste("SELECT measurement_id, person_id, measurement_date, measurement_type_concept_id, value_as_number, value_as_concept_id FROM @cdmSchema.measurement WHERE person_id=",as.character(cases_pids[patientQueue,1]),";",sep='')
            renderedSql <- renderSql(getPatientLabs, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
            translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
            tmp_fv <- dbGetQueryPostgreSql(conn,translatedSql)
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
            patientFeatures_cases_p_labs_df[[patientQueue]]<-test1
            rm('test1')          
            rm('tmp_fv')     
          }
      }
      
      if (flag_observations) {
        save(patientFeatures_cases_p_drugexposures_df,file=paste("RAW_FV_PREDICT_drugexposures_",as.character(nCases),".Rda",sep=''))
      }
      if (flag_visits) {
        save(patientFeatures_cases_p_visits_df,file=paste("RAW_FV_PREDICT_visits_",as.character(nCases),".Rda",sep=''))
      }
      if (flag_observations) {
        save(patientFeatures_cases_p_observations_df,file=paste("RAW_FV_PREDICT_observations_",as.character(nCases),".Rda",sep=''))
      }
      
      if (flag_labs) {
        save(patientFeatures_cases_p_labs_df,file=paste("RAW_FV_PREDICT_Labs_",as.character(nCases),".Rda",sep=''))
      }

} else { #Running on saved patients

      load("~/Phenotyping/RAW_FV_PREDICT_Labs_10.Rda")
      load("~/Phenotyping/RAW_FV_PREDICT_observations_10.Rda")
      load("~/Phenotyping/RAW_FV_PREDICT_drugexposures_10.Rda")
      load("~/Phenotyping/RAW_FV_PREDICT_visits_10.Rda")

}
dbDisconnect(conn)

if (flag_observations) {
  test<-patientFeatures_cases_p_observations_df
  FV_ob_p <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_ob_p <- ddply(FV_ob_p, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
  colnames(FV_ob_p)<-paste('obs:',colnames(FV_ob_p),sep='')
  colnames(FV_ob_p)[1]<-"pid"
  rm(test)
}

#Get the Patients FV for Visits
if (flag_visits) {
  test<-patientFeatures_cases_p_visits_df
  FV_v_p <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_v_p <- ddply(FV_v_p, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
  colnames(FV_v_p)<-paste('visit:',colnames(FV_v_p),sep='')
  colnames(FV_v_p)[1]<-"pid"
  rm(test)
}
#Get the Patients FV for drug exposures
if (flag_drugexposures) {
  test<-patientFeatures_cases_p_drugexposures_df
  FV_de_p <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_de_p <- ddply(FV_de_p, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
  colnames(FV_de_p)<-paste('drugexp:',colnames(FV_de_p),sep='')
  colnames(FV_de_p)[1]<-"pid"
  rm(test)
}

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

if (flag_labs) {
  test<-patientFeatures_cases_labs_df_n
  FV_lab <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
  FV_lab[is.na(FV_lab)]<-0
  colnames(FV_lab)<-paste('lab:',colnames(FV_lab),sep='')
  colnames(FV_lab)[1]<-"pid"
  rm(test)
}



feature_vectors_p <- list()

featuresets=1
if (flag_drugexposures) {
  feature_vectors_p[[featuresets]]<-FV_de_p
  featuresets = featuresets+1
}
if (flag_visits) {
  feature_vectors_p[[featuresets]]<-FV_v_p
  featuresets = featuresets+1
}
if (flag_observations) {
  feature_vectors_p[[featuresets]]<-FV_ob_p
  featuresets = featuresets+1
}
if (flag_labs) {
  feature_vectors_p[[featuresets]]<-FV_labs_p
  featuresets = featuresets+1
}

#Merge all dataframes/Feature vectors for the different sources and have a big list of them
pp_total = Reduce(function(...) merge(..., by="pid", all=T), feature_vectors_p)
#Get selection of FV we wnat
ppv_set_p<-pp_total[1:nrow(pp_total),2:ncol(pp_total)]
ppv_set_p<-pp_total
#Convert features to boolean or leave as bag
if (tolower(c(features_mode)) == 'boolean') {
  ppv_set_p[ppv_set_p > 0] <- 1  #
}

#Load the model and the list of predictors
load(paste(flag_model, " MODEL FILE FOR ",outcomeName,".Rda",sep=''))
load(paste(flag_model, " PREDICTORS FOR ",outcomeName,".Rda",sep=''))
#Load the model and the list of predictors

#Now we need to make sure the feature vectors align with the ones used for the model
predicteesNames <- names(ppv_set_p)
toRemove<-predicteesNames[!(predicteesNames %in% predictorsNames)]
missing<-predictorsNames[!(predictorsNames %in% predicteesNames)]   #Find the missing columns to add to the new FV
predictDF <- ppv_set_p[,!(predicteesNames %in% toRemove)]
#Add all the missing features
predictDF[c(missing)] <- 0
#attr(predictDF, "row.names") <- c(cases_pids$V1)
#predictDF<-ppv_set_p[,predictorsNames]


## Prediction section ###
if (flag_model=='LASSO') {
  ################################################
  # glmnet LASSO                                ##
  ################################################
  # get predictions on data
  predictions <- predict(object=objModel, newdata = predictDF[,predictorsNames])
  #Define predictions
  RMSE = min(objModel$results$RMSE)
  predictionResults<-cases_pids
  predictionResults$V2 <-predictions
  #This contains the prediction
  predictionResults$V3 <- predictionResults$V2 >= RMSE
  predictionResults <- predictionResults[c("V1", "V3")]
  colnames(predictionResults)[1] <- "patient_id"
  colnames(predictionResults)[2] <- "prediction"
  ###### Model Ouputs to file #############
  sink(paste('PREDICTION LASSO output for-',outcomeName,'-Cases-',as.character(nCases),'-Controls-',as.character(nControls),'.txt',sep=''))
  cat(paste('Prediction Results for LASSO Model for-',outcomeName,' using ',as.character(nCases),' Cases and ',as.character(nControls),' Controls. \n\n',sep=''))
  cat("\n")
  print(predictionResults)
  cat("\n")
  cat("\nGenerated on ")
  cat(format(Sys.time(), "%a %b %d %Y %X"))
  sink()
}

if (flag_model=='RF') {
  ###########################################
  ### Random Forest #########################
  ###########################################
  predictions <- predict(object=objModel, newdata = predictDF[,predictorsNames])
  predictionResults<-cases_pids
  predictionResults$V2 <-predictions
  colnames(predictionResults)[1] <- "patient_id"
  colnames(predictionResults)[2] <- "prediction"  
  
  sink(paste('PREDICTION RF output for-',outcomeName,'-Cases-',as.character(nCases),'-Controls-',as.character(nControls),'.txt',sep=''))
  cat(paste('Prediction results for RF Model for-',outcomeName,' using ',as.character(nCases),' Cases and ',as.character(nControls),' Controls. \n\n',sep=''))
  cat("\n")
  print(predictionResults)
  cat("\nGenerated on ")
  cat(format(Sys.time(), "%a %b %d %Y %X"))
  sink()
}
