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

# Command line argument required: name of settings file 
# e.g."Aphrodite/R/settings_knTesting_FH.R"
#args <- commandArgs(trailingOnly = TRUE)
#message(args)


folder = "/home/kniehaus/Aphrodite/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

saveFolder = "/home/kniehaus/Intermediate_data/"

source("R/settings_knTesting_FH_14Jul2015.R")   #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it
#source(args[1])
source("R/functions.R")     # source this if changes have been made that aren't yet in the package

#Initiate connection to DB
connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)


# --------------------------------------------------------------------------------------------
# STEP 1 - Generate Keywords
# --------------------------------------------------------------------------------------------
wordLists <- buildKeywordList(conn, aphrodite_concept_name, cdmSchema, dbms)

write.table(wordLists$keywordlist_ALL, file=paste(saveFolder,studyName,'_keywordlist.tsv',sep=''), quote=FALSE, sep='~', row.names = FALSE, col.names = FALSE)
write.table(wordLists$ignorelist_ALL, file=paste(saveFolder,studyName, '_ignorelist.tsv',sep=''), quote=FALSE, sep='~', row.names = FALSE, col.names = FALSE)

message(paste("keywordlist.tsv and ignorelist.tsv have been successfully created for ",aphrodite_concept_name,sep = ""))
message("Please edit these files and save as keywordlist_ed.tsv and ignorelist_ed.tsv")


# CLEAN UP

#Close connection
dbDisconnect(conn)


