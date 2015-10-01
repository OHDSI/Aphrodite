# Copyright 2015 Observational Health Data Sciences and Informatics
#
# This file is part of:
#
#   █████╗ ██████╗ ██╗  ██╗██████╗  ██████╗ ██████╗ ██╗████████╗███████╗
#   ██╔══██╗██╔══██╗██║  ██║██╔══██╗██╔═══██╗██╔══██╗██║╚══██╔══╝██╔════╝
#   ███████║██████╔╝███████║██████╔╝██║   ██║██║  ██║██║   ██║   █████╗
#   ██╔══██║██╔═══╝ ██╔══██║██╔══██╗██║   ██║██║  ██║██║   ██║   ██╔══╝
#   ██║  ██║██║     ██║  ██║██║  ██║╚██████╔╝██████╔╝██║   ██║   ███████╗
#   ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚═════╝ ╚═╝   ╚═╝   ╚══════╝
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Stanford University Center for Biomedical Informatics - Shah Lab
# @author Juan M. Banda

# Install necessary packages if needed, remove comments
# install.packages("devtools")
# install_github("ohdsi/Aphrodite")

library(Aphrodite)
library(SqlRender)
library(plyr)
library(caret)
library(pROC)
library(data.table)
library(DatabaseConnector)
library(ggplot2)


# Initiate connection to DB

#jdbcDrivers <<- new.env()   #In case you get a connection error uncomment this line

folder = "/home/jmbanda/OHDSI/Aphrodite-TEMP/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

source("CopyOfsettings.R")  #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it


connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)

##################################################################################
### STEP 1 - Generate Keywords                                                 ###
##################################################################################

wordLists <- buildKeywordList(conn, aphrodite_concept_name, cdmSchema, dbms)

write.table(wordLists$keywordlist_ALL, file=paste('keywordlistAF.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
write.table(wordLists$ignorelist_ALL, file=paste('ignorelistAF.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)

message(paste("Keywords.tsv and ignore.tsv have been successfully created for ",aphrodite_concept_name,sep = ""))

##################################################################################
### NOTICE: Do not forget to edit the keywords and ignore files                ###
##################################################################################

# Load Keyword list after editing
keywordList_FF <- read.table('keywordlistAF.tsv', sep="\t", header=FALSE)
ignoreList_FF <- read.table('ignorelistAF.tsv', sep="\t", header=FALSE)

##################################################################################
### STEP 2 - Look for cases and controls in the patient data                   ###
##################################################################################

casesANDcontrolspatient_ids_df<- getdPatientCohort(conn, dbms,as.character(keywordList_FF$V3),as.character(ignoreList_FF$V3), cdmSchema,nCases,nControls)
if (nCases > nrow(casesANDcontrolspatient_ids_df[[1]])) {
    message("Not enough patients to get the number of cases specified")
    stop
} else {
    if (nCases > nrow(casesANDcontrolspatient_ids_df[[2]])) {
        message("Not enough patients to get the number of controls specified")
        stop
    }
}

cases<- casesANDcontrolspatient_ids_df[[1]][sample(nrow(casesANDcontrolspatient_ids_df[[1]]), nCases), ]
controls<- casesANDcontrolspatient_ids_df[[2]][sample(nrow(casesANDcontrolspatient_ids_df[[2]]), nControls), ]

if (saveALLresults) {
    write.table(cases, file=paste('casesAF.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
    write.table(controls, file=paste('controlsAF.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
}

##################################################################################
### Get cases data                                                             ###
##################################################################################
# IF we want to restrict domain id's we use
# dataFcases <- getPatientData(conn, dbms, cases, as.character(ignoreList_FF$V3), flag, cdmSchema, flag$remove_domains[1])
# For the example this will be Unit that we are removing

dataFcases <- getPatientData(conn, dbms, cases, as.character(ignoreList_FF$V3), flag, cdmSchema)
if (saveALLresults) {
    save(dataFcases,file=paste(studyName,"-RAW_FV_CASES_",as.character(nCases),".Rda",sep=''))
}

##################################################################################
### Get control data                                                           ###
##################################################################################

dataFcontrols <- getPatientData(conn, dbms, controls, as.character(ignoreList_FF$V3), flag, cdmSchema)
if (saveALLresults) {
    save(dataFcontrols,file=paste(studyName,"-RAW_FV_CONTROLS_",as.character(nControls),".Rda",sep=''))
}

##################################################################################
### Create feature vector                                                      ###
##################################################################################

fv_all<-buildFeatureVector(flag, dataFcases,dataFcontrols)
fv_full_data <- combineFeatureVectors(flag, data.frame(cases), controls, fv_all, outcomeName)

if (saveALLresults) {
    save(fv_all,file=paste(studyName,"-FULL_FV_CASES_",as.character(nCases),"_CONTROLS_",as.character(nControls),".Rda",sep=''))
}

charCols <- c("Class_labels", "pid")
predictorsNames <- colnames(fv_full_data)[!colnames(fv_full_data) %in% charCols]

#### Remove for demo
# check that all data is real
max(fv_full_data[,predictorsNames])
fullFeatDist <- as.numeric(unlist(fv_full_data[,predictorsNames]))

##################################################################################
### STEP 3 - Create model                                                      ###
##################################################################################

model_predictors <- buildModel(flag, fv_full_data, outcomeName, folder)
model<-model_predictors$model
predictorsNames<-model_predictors$predictorsNames
auc <- model_predictors$auc
# save model
save(model, file=paste(folder,studyName,'_model_', flag$model[1], '_', outcomeName,".Rda",sep=''))
#Save Predictors for model
save(predictorsNames, file=paste(folder,studyName,'_predictors_',flag$model[1], '_', outcomeName, ".Rda",sep=''))

dbDisconnect(conn)
