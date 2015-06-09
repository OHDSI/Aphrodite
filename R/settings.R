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

cdmSchema = ""
resultsSchema = ""
sourceName = ""
dbms = "" #Should be "sql server", "oracle", "postgresql" or "redshift"

user <- ""
pw <- ""
server <- ""
port <- ""

###################################
### Aphrodite package variables ###
###################################

studyName <-'MI'   #String for saving objects prefix
outcomeName <- 'MI' #String for model objects prefix
nCases = 20 # Number of patients to use as cases
nControls = 20 #Number of patients to use as controls

### CONCEPT to build a Phenotype for ###
aphrodite_concept_name <- "myocardial infarction"

#### Flags ###
flag <- data.frame(drugexposures= integer(1), observations= integer(1),visits=integer(1), labs=integer(1), model=character(1), feature_mode = character(1), stringsAsFactors=FALSE)
flag$drugexposures[1]=1   #Use drug_exposures as features  (1 yes, 0 no)
flag$observations[1]=1    #Use observations as features  (1 yes, 0 no)
flag$visits[1]=1          #Use visits () as features  (1 yes, 0 no)
flag$labs[1]=0            #Use labs as features  (1 yes, 0 no)

### Save intermediate Results Flag ###
saveALLresults=1   # There is a cleaning function to remove all intermediate results

### Type of model to build  ###
flag$model[1] <-'LASSO' # Available types: LASSO - mode will be added in the future
### How to use features ###
flag$features_mode[1] <- 'frequency'   #Can be Boolean or frequency - boolean only requires presence and frequency keeps the counts
### Do we want to plot the AUOC and Top Variables plots ###
plots=1

##################################################################################
### Predictions file - This is only needed when you are testing a built model. ###
##################################################################################
toPredict <- "cases_MI.txt"
