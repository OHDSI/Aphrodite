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



folder = "/home/kniehaus/Aphrodite/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

source("/home/kniehaus/Intermediate_data/FH_test200_3/settings_knTesting_FH_15Jul2015.R")   #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it
source("R/functions.R")     # source this if changes have been made that aren't yet in the package

#Initiate connection to DB
jdbcDrivers <<- new.env()
connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)


# --------------------------------------------------------------------------------------------
# STEP 1 - Generate Keywords
# --------------------------------------------------------------------------------------------
wordLists <- buildKeywordList(conn, aphrodite_concept_name, cdmSchema, dbms)

write.table(wordLists$keywordlist_ALL, file=paste(saveFolder,studyName,'_keywordlist.csv',sep=''), quote=FALSE, sep=',', row.names = FALSE, col.names = FALSE)
write.table(wordLists$ignorelist_ALL, file=paste(saveFolder,studyName, '_ignorelist.csv',sep=''), quote=FALSE, sep=',', row.names = FALSE, col.names = FALSE)

message(paste("keywordlist.tsv and ignorelist.tsv have been successfully created for ",aphrodite_concept_name,sep = ""))
message("Please edit these files and save as keywordlist_ed.tsv and ignorelist_caseID, ignorelist_features.tsv")


# CLEAN UP

#Close connection
dbDisconnect(conn)


