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
flag <- data.frame(drugexposures= integer(1), observations= integer(1),visits=integer(1), labs=integer(1), model=character(1), features_mode = character(1), stringsAsFactors=FALSE, remove_domains=(""), timeWindowOpt=integer(1), threshCutoff = .02, timeNormalize=integer(1))
flag$drugexposures[1]=1   #Use drug_exposures as features  (1 yes, 0 no)
flag$observations[1]=1    #Use observations as features  (1 yes, 0 no)
flag$visits[1]=1          #Use visits () as features  (1 yes, 0 no)
flag$labs[1]=0            #Use labs as features  (1 yes, 0 no)

### Options to define range of feature variables for cases
# Time window from which to grab features
flag$timeWindowOpt[1] <- 1
# 1=go time of first keyword appearance in notes --> last keyword appearance
# 2=go 10 years before first keyword appearance --> first keyword appearance

# Filter for feature inclusion - discard any features found in less than threshCutoff proportion of sample set
flag$threshCutoff[1] <- 0.05

# Normalize by the length of patient follow-up
flag$timeNormalize[1] <- 0
# 0 = no normalization
# 1 = normalize by length of patient follow-up in terms of years (if only a single visit, divide by 1 year)
# 2 = normalize by length of patient follow-up in terms of months (if only a single visit, divide by 1 month)
# 3 = normalize by number of visits
# 4 = normalize by number of measurement codes, in that category



### Save intermediate Results Flag ###
saveALLresults=1   # There is a cleaning function to remove all intermediate results

### Type of model to build  ###
# Available types:
# LASSO = logistic regression with elastic net regularization (optimizes over elastic net parameter, alpha; 0=ridge regularization=L2 norm, 1=lasso regularization=L1 norm)
# RF = random forest
flag$model[1] <-'LASSO'



### How to use features ###
flag$features_mode[1] <- 'frequency'   #Can be Boolean or frequency - boolean only requires presence and frequency keeps the counts

flag$remove_domains[1] <- ("'Unit'") # can choose whether to remove concept domains if finding irrelevant features
#This needs to be passed explicitly to the getPatientData function if needed, see the example for more details
# e.g. flag$remove_domains[1] <- c("'Metadata'", "'Unit'", "'Meas Value'", "'Note Type'")


##################################################################################
### Plots                                                                      ###
##################################################################################
### Do we want to plot the AUOC and Top Variables plots ###
plots=1
### How many features to include in top variables plot
numFeats <- 30
##################################################################################
### Predictions file - This is only needed when you are testing a built model. ###
##################################################################################
toPredict <- "cases_MI.txt"
