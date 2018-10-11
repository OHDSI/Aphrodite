# APHRODITE

Automated PHenotype Routine for Observational Definition, Identification, Training and Evaluation (APHRODITE)

Typically, patient groups corresponding to a phenotype are selected by rule-based definitions, whose development is time-consuming. Machine learning approaches, which are an alternative approach for electronic phenotyping, are hampered by the need of manually labeled gold standard training sets.

We demonstrate the feasibility of using imperfectly labeled training sets to construct phenotype models. Our approach uses heuristic labeling strategy ("noisy" labeling), in conjunction with expert knowledge from existing ontologies, and a comprehensive representation of the patient clinical record to learn phenotype models.

Reference: http://www.ncbi.nlm.nih.gov/pubmed/27174893

Want to cite APHRODITE? Please cite this paper: https://www.ncbi.nlm.nih.gov/pubmed/28815104

How to video: http://tinyurl.com/use-aphrodite

Minimum Requirements
===================
- CDM v5
- R Packages:
	- caret
	- shiny
	- dplyr
	- pROC
    - data.table
	- devtools
	- SqlRender
	- DatabaseConnector

Installation
===================

```
install_github("ohdsi/Aphrodite")
```

Loading
===================

```
library(Aphrodite)
```

Before you start
===================

You need to update the /R/settings.R file with your CDM connection information and Phenotyping settings. Example runs for Myocardial Infarction

Developed and Tested
===================

Aphrodite was developed using R version 3.1.2. Fully tested with CDMv5 and Vocabulary V5 on a Postgres DB server.

Full Example
===================

This file is also found under /samples/complete_example.R

```r
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


```

Anchors support
===================

Support for Anchors has been added and is exmplained in the follwing complete example: /samples/complete_example_anchors.R

Getting Involved
=============
* Package manual: [Aphrodite manual](https://github.com/OHDSI/Aphrodite/raw/master/man/Aphrodite.pdf) 
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements

License
=======
Aphrodite is licensed under Apache License 2.0

Development
============
Aphrodite is being developed in R Studio.

