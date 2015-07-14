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
# @author Kate Niehaus
# (building upon script by JMBanda)

# --------------------------------------------------------------------------------------------
# Pre-stuff
# --------------------------------------------------------------------------------------------

# Install necessary packages if needed (remove comments to do so)
#install.packages("devtools")
#library(devtools)
#install_github("ohdsi/Aphrodite")
library(Aphrodite)
library(SqlRender)
library(plyr)
library(caret)
library(pROC)
library(DatabaseConnector)
library(ggplot2)
library(data.table)
library(RJDBC)

# Command line argument required: name of settings file 
# e.g."Aphrodite/R/settings_knTesting_FH.R"
#args <- commandArgs(trailingOnly = TRUE)

folder = "/home/kniehaus/Aphrodite/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)
source("R/settings_knTesting_FH_14Jul2015.R")   #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it
#source(args[1])
source("R/functions.R")     # source this if changes have been made that aren't yet in the package

#Initiate connection to DB
if (connNeeded) {
  connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
  conn <- connect(connectionDetails)
}


# --------------------------------------------------------------------------------------------
# STEP 1 - Load edited Keywords
# --------------------------------------------------------------------------------------------

# Load Keyword list after editing
keywordList_FF <- read.table(paste(saveFolder,studyName,'_keywordlist_ed.tsv',sep=''), sep="\t", header=FALSE)
ignoreList_FF <- read.table(paste(saveFolder,studyName, '_ignorelist_ed.tsv',sep=''), sep="\t", header=FALSE)

message("Keyword lists re-loaded")

# --------------------------------------------------------------------------------------------
# STEP 2 - Get cases, controls
# --------------------------------------------------------------------------------------------
# This returns nControls, but all possible cases (> nCases (usually))
# Returns casesANDcontrolspatient_ids_df = [cases, controls]


if (loadCases) {
  # Load saved pids
  cases <-read.table(paste(saveFolder,studyName,'_cases.tsv',sep=''))
  controls <- read.table(paste(saveFolder,studyName,'_controls.tsv',sep=''))
} else {
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
  
  set.seed(123)
  cases<- casesANDcontrolspatient_ids_df[[1]][sample(nrow(casesANDcontrolspatient_ids_df[[1]]), nCases), ]
  controls<- casesANDcontrolspatient_ids_df[[2]][sample(nrow(casesANDcontrolspatient_ids_df[[2]]), nControls), ]
  
  if (saveALLresults) {
      write.table(cases, file=paste(saveFolder,studyName,'_cases.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
      write.table(controls, file=paste(saveFolder,studyName,'_controls.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
  }
  
  message("Cases and controls identified")
}

# --------------------------------------------------------------------------------------------
# STEP 3 - Get Patient Data
# --------------------------------------------------------------------------------------------
#Cases needs its own function b/c looking after time of dx 

caseDataFN <-paste(saveFolder,studyName,"_RAW_FV_CASES_",as.character(nCases),".Rda",sep='')
controlDataFN <-paste(saveFolder,studyName,"_RAW_FV_CONTROLS_",as.character(nControls),".Rda",sep='')

if (loadPtData) {
  load(caseDataFN)
  load(controlDataFN)
} else {
  # TODO: need to add option to look before dx, too
  # TODO: combine these two into a single function
  dataFcases <-getPatientDataCases(conn, dbms, cases, as.character(keywordList_FF$V3),as.character(ignoreList_FF$V3), flag , cdmSchema)
  if (saveALLresults) {
      save(dataFcases,file=caseDataFN)
  }
  
  #Get Controls
  dataFcontrols <- getPatientData(conn, dbms, controls, flag , cdmSchema)
  if (saveALLresults) {
      save(dataFcontrols,file=controlDataFN)
  }
  
  message("Cases and controls data extracted")
}

# --------------------------------------------------------------------------------------------
# STEP 4 - Build feature vectors
# --------------------------------------------------------------------------------------------
# Very long step!!

if (loadFeatVector) {
  # load vector from file - loads as fv_all
  load(file=paste(saveFolder,studyName,"_FULL_FV_CASES_",as.character(nCases),"_CONTROLS_",as.character(nControls),".Rda",sep=''))
  message("Feature vector loaded")
} else {
  # create feature vector
  fv_all<-buildFeatureVector(flag, dataFcases,dataFcontrols)
  if (saveALLresults) {
      save(fv_all,file=paste(saveFolder,studyName,"_FULL_FV_CASES_",as.character(nCases),"_CONTROLS_",as.character(nControls),".Rda",sep=''))
  }
  message("Feature vector built")
}



# --------------------------------------------------------------------------------------------
# Step 5 - Build Model
# --------------------------------------------------------------------------------------------
#colnames(fv_all[[3]])[1] <- "pid"

if(loadModel) {
  # load saved model
  # loads model
  #e.g.load('~/Intermediate_data/LASSO MODEL FILE FOR FH_test.Rda')
  load(file=paste(saveFolder,studyName,'_model_', flag$model[1], '_', outcomeName,".Rda",sep=''))
  
  # loads predictors
  load(file=paste(saveFolder,studyName,'_predictors_',flag$model[1], '_', outcomeName, ".Rda",sep=''))
  
  message("Model loaded")
  
} else {
  # create model
  model_predictors <- buildModel(flag, cases, controls, fv_all, outcomeName, saveFolder)
  
  message("Model built")
  
  ###### Save Model to file #############
  model<-model_predictors$model
  predictors<-model_predictors$predictors
  auc <- model_predictors$auc
  
  # new_coef <- coef(model$finalModel, model$bestTune$lambda)
  # b <- model$finalModel$beta
  
  save(model, file=paste(saveFolder,studyName,'_model_', flag$model[1], '_', outcomeName,".Rda",sep=''))
  #Save Predictors for model
  save(predictors, file=paste(saveFolder,studyName,'_predictors_',flag$model[1], '_', outcomeName, ".Rda",sep=''))
  save(auc, file=paste(saveFolder,studyName,'_auc_',flag$model[1], '_', outcomeName, ".Rda",sep=''))
}



# --------------------------------------------------------------------------------------------
# STEP 6 - Evaluate model
# --------------------------------------------------------------------------------------------

if (loadWeightings) {
  load(file=paste(saveFolder, studyName, "_concepts_", numFeats, ".Rda",sep=''))
} else {
  # Look at the top-ranking features
  weightingsDF <- conceptDecoder(conn, cdmSchema, dbms, model, numFeats)

  # save concepts
  save(weightingsDF, file=paste(saveFolder, studyName, "_concepts_", numFeats, ".Rda",sep=''))
}

# plot weightings
plotSaveFile <- paste(saveFolder,studyName,'_featWeight_plot_', nCases, '_', nControls,'.jpg',sep='')
plotFeatWeightings(plotSaveFile, weightingsDF)



# Evaluate for patients who have been gold-standard labeled

# **KEEP BELOW** - code used to get pid list from patient list
# Get pid list
# Must do from dev2 to get pid list (need to only do once):
# con = dbConnect(MySQL(), user='kniehaus', password='bmir1234', dbname='user_kniehaus', host='shahlab-db1.stanford.edu')
# queryString <- "SELECT pids.* FROM fh_pids as pids"
# queryObject <- dbSendQuery(con,queryString)
# test_pids <- fetch(queryObject,n=-1);
# dbDisconnect(con);
# write.table(test_pids, file=paste('~/Intermediate_data/FH_GS_phenotype_pids.csv',sep=''), quote=FALSE, sep=',', row.names = FALSE, col.names = FALSE)


test_pids <- read.csv(toPredict)
testProbs <- testModel(conn, cdmSchema, dbms, model, test_pids, flag)



# CLEAN UP

#Close connection
dbDisconnect(conn)


