# Aphrodite

Automated PHenotype Routine for Observational Definition, Identification, Training and Evaluation (APHRODITE) - Previously known as XPRESS. 

Typically, patient groups corresponding to a phenotype are selected by rule-based definitions, whose development is time-consuming. Machine learning approaches, which are an alternative approach for electronic phenotyping, are hampered by the paucity of manually labeled gold standard corpora for training. We demonstrate the feasibility of using noisy class labels to create “silver standard” training corpora to construct phenotype models. Our approach uses such silver standard corpora, in conjunction with expert knowledge codified in existing ontologies and a comprehensive representation of the patient clinical record, to learn phenotype models.

Reference (for now): http://www.dmmh.org/dmmi2014_submission_4.pdf?attredirects=0&d=1

Requirementes
===================
- CDM v5
- R Packages:
	- caret
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

You need to udpate the /R/settings.R file with your CDM connection information and Phenotyping settings.

Full Example
===================

This file is also found under /samples/complete_example.R

```r
library(Aphrodite)

folder = "/home/jmbanda/OHDSI/Aphrodite-TEMP/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

source("CopyOfsettings.R")   #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it

connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)

# load up files with keyword lists
# load list of terms to ignore as features

# STEP 1 - Generate Keywords
wordLists <- buildKeywordList(conn, aphrodite_concept_name, cdmSchema, dbms)

write.table(wordLists$keywordlist_ALL, file=paste('keywordlist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
write.table(wordLists$ignorelist_ALL, file=paste('ignorelist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)

message(paste("Keywords.tsv and ignore.tsv have been successfully created for ",aphrodite_concept_name,sep = ""))

# Load Keyword list after editing
keywordList_FF <- read.table('keywordlist.tsv', sep="\t", header=FALSE)
ignoreList_feat <- read.table('ignorelist.tsv', sep="\t", header=FALSE)

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


# filename to use for saving case data
dataFcases <- getPatientData(conn, dbms, cases, as.character(ignoreList_feat$V3), flag, cdmSchema)
if (saveALLresults) {
    save(dataFcases,file=paste(studyName,"-RAW_FV_CASES_",as.character(nCases),".Rda",sep=''))
}

##################################################################################
### Get control data ###
##################################################################################

dataFcontrols <- getPatientData(conn, dbms, controls, as.character(ignoreList_feat$V3), flag, cdmSchema)
if (saveALLresults) {
    save(dataFcontrols,file=paste(studyName,"-RAW_FV_CONTROLS_",as.character(nControls),".Rda",sep=''))
}

##################################################################################
### Create feature vector ###
##################################################################################

fv_all<-buildFeatureVector(flag, dataFcases,dataFcontrols)




fv_full_data <- combineFeatureVectors(flag, data.frame(cases), controls, fv_all, outcomeName)

# save data

if (saveALLresults) {
    save(fv_all,file=paste(studyName,"-FULL_FV_CASES_",as.character(nCases),"_CONTROLS_",as.character(nControls),".Rda",sep=''))
}
#    save(fv_all,file=paste(saveFolder,studyName,"_FULL_FV_pre.Rda",sep=''))
#    save(fv_full_data,file=paste(saveFolder,studyName,"_FULL_FV_final.Rda", sep=''))



charCols <- c("Class_labels", "pid")
predictorsNames <- colnames(fv_full_data)[!colnames(fv_full_data) %in% charCols]
# check that all data is real
max(fv_full_data[,predictorsNames])
fullFeatDist <- as.numeric(unlist(fv_full_data[,predictorsNames]))


##################################################################################
### Create model ###
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

