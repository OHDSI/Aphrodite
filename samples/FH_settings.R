# @file settings
#
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

######################################################################################
###         Settings.R - Modify only when building a new phenotype                 ###
######################################################################################


### Connection Variables ###

cdmSchema = "ohdsiv5"
resultsSchema = "ohdsiv5"
sourceName = ""
dbms = "postgresql" #Should be "sql server", "oracle", "postgresql" or "redshift"

user <- "achilles"
pw <- "achilles"
server <- "localhost/jmbanda"
port <- "5432"



###################################
### Aphrodite package variables ###
###################################
studyName <-'FH_GS'   #String for saving objects prefix
outcomeName <- 'FH_GS' #String for model objects prefix
nControls = 400 #Number of patients to use as controls

### CONCEPT to build a Phenotype for ###
#aphrodite_concept_name <- "myocardial infarction"
aphrodite_concept_name <- "familial hypercholesterolemia"

#### Flags ###
flag <- data.frame(drugexposures= integer(1), observations= integer(1),visits=integer(1), labs=integer(1), model=character(1), features_mode = character(1), stringsAsFactors=FALSE, remove_domains=(""), timeWindowOpt=integer(1), threshCutoff = .2, timeNormalize=integer(1))
flag$drugexposures[1]=1   #Use drug_exposures as features  (1 yes, 0 no)
flag$observations[1]=0    #Use observations (text in notes) as features  (1 yes, 0 no)
flag$visits[1]=1          #Use visits (icd9, cpt codes) as features  (1 yes, 0 no)
flag$labs[1]=1           #Use labs as features  (1 yes, 0 no)

### Options to define range of feature variables for cases
# Time window from which to grab features
flag$timeWindowOpt[1] <- 1
# 1=go time of first keyword appearance in notes --> last keyword appearance
# 2=go 10 years before first keyword appearance --> first keyword appearance

# Filter for feature inclusion - discard any features found in less than threshCutoff proportion of sample set
flag$threshCutoff[1] <- 0.02

# Normalize by the length of patient follow-up
flag$timeNormalize[1] <- 1
# 1 = normalize by length of patient follow-up (if only a single visit, divide by 1)
# 2 = do not normalize by length of patient follow-up


### Type of model to build  ###
flag$model[1] <-'RF' 
# Available types: 
# LASSO = logistic regression with elastic net regularization (optimizes over elastic net parameter, alpha; 0=ridge regularization=L2 norm, 1=lasso regularization=L1 norm)
# RF = random forest


### How to use features ###
flag$features_mode[1] <- 'frequency'   #Can be Boolean or frequency - boolean only requires presence and frequency keeps the counts

flag$remove_domains[1] <- ("''")
# can choose whether to remove concept domains if finding irrelevant features
# e.g. flag$remove_domains[1] <- c("'Metadata'", "'Unit'", "'Meas Value'", "'Note Type'")

### Do we want to plot the AUOC and Top Variables plots ###
plots=1

### How many features to include in top variables plot
numFeats <- 30

##################################################################################
### Predictions file - This is only needed when you are testing a built model. ###
##################################################################################
GS_pts <- "/home/kniehaus/Intermediate_data/FH_pids_updated_20Jul2015.csv"


###################################
### Stage of analysis ###
###################################

### Save intermediate Results Flag ###
saveALLresults=1   # There is a cleaning function to remove all intermediate results
#TODO: build this function
saveFolder = "/home/kniehaus/Intermediate_data/FH_GS_01/"

# Variables to specify which steps still need to be run, if going from saved data
connNeeded=1  #1=connection to database is needed; 0=no connection needed
loadControls=1   # 1=load list of cases/controls from saved file; 0=perform processing to obtain this list
loadCaseData=1    # 1=load lists of pt data from saved file; 0=perform processing to obtain this list
loadControlData=1   #1=load fontrol data from saved ile; 0=perform processing to obtain tis list
loadFeatVector=1 # 1=load feature vector of pt data from saved file; 0=perform processing to obtain this vector
loadModel=0     # 1=load model from saved file; 0=perform training for model
loadWeightings=0    # 1=load feature weightings from saved file; 0=perform processing to obtain weightings DF





