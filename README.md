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

#Initiate connection
connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)

# STEP 1 - Generate Keywords
wordLists <- buildKeywordList(conn, aphrodite_concept_name, cdmSchema, dbms)

write.table(wordLists$keywordlist_ALL, file=paste('keywordlist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
write.table(wordLists$ignorelist_ALL, file=paste('ignorelist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)

message(paste("Keywords.tsv and ignore.tsv have been successfully created for ",aphrodite_concept_name,sep = ""))

####################################################################
##NOTE: After generating the keywords manual curation is desired  ##
####################################################################

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
dataFcontrols <- getPatientData(conn, dbms, controls, flag , cdmSchema)
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

