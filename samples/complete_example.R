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
#install_github("ohdsi/Aphrodite")
library(Aphrodite)
library(SqlRender)
library(plyr)
library(caret)
library(pROC)
library(data.table)
library(DatabaseConnector)
folder = "/home/jmbanda/OHDSI/Aphrodite-TEMP/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

source("CopyOfsettings.R")   #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it

#Initiate connection

jdbcDrivers <<- new.env()

connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)

# STEP 1 - Generate Keywords
wordLists <- buildKeywordList(conn, aphrodite_concept_name, cdmSchema, dbms)

write.table(wordLists$keywordlist_ALL, file=paste('keywordlist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
write.table(wordLists$ignorelist_ALL, file=paste('ignorelist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)

message(paste("Keywords.tsv and ignore.tsv have been successfully created for ",aphrodite_concept_name,sep = ""))

# Load Keyword list after editing
keywordList_FF <- read.table('keywordlist.tsv', sep="\t", header=FALSE)
ignoreList_FF <- read.table('ignorelist.tsv', sep="\t", header=FALSE)


# STEP 2 - Get cases, controls

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
    write.table(cases, file=paste('cases.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
    write.table(controls, file=paste('controls.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
}

# STEP 3 - Get Patient Data

#Cases needs its own function
dataFcases <-getPatientDataCases(conn, dbms, cases, as.character(keywordList_FF$V3),as.character(ignoreList_FF$V3), flag , cdmSchema)
if (saveALLresults) {
    save(dataFcases,file=paste(studyName,"-RAW_FV_CASES_",as.character(nCases),".Rda",sep=''))
}

#Get Controls
dataFcontrols <- getPatientData(conn, dbms, controls, as.character(ignoreList_FF$V3), flag, cdmSchema)
#dataFcontrols <- getPatientData(conn, dbms, controls, flag , cdmSchema)
if (saveALLresults) {
    save(dataFcontrols,file=paste(studyName,"-RAW_FV_CONTROLS_",as.character(nControls),".Rda",sep=''))
}

# Build feature vectors
fv_all<-buildFeatureVector(flag, dataFcases,dataFcontrols)
if (saveALLresults) {
    save(fv_all,file=paste(studyName,"-FULL_FV_CASES_",as.character(nCases),"_CONTROLS_",as.character(nControls),".Rda",sep=''))
}

# Step 4 - Build Model

model_predictors <- buildModel(flag, cases, controls, fv_all, outcomeName)

###### Save Model to file #############
model<-model_predictors$model
predictors<-model_predictors$predictors

save(model, file=paste(flag$model[1], " MODEL FILE FOR ",outcomeName,".Rda",sep=''))
#Save Predictors for model
save(predictors, file=paste(flag$model[1], " PREDICTORS FOR ",outcomeName,".Rda",sep=''))

#Close connection
dbDisconnect(conn)




