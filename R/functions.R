# @file functions
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


##################################################################################
## executeSQL  - This function executes one single SQL statement                ##
##                                                                              ##
##                                                                              ##
## Arguments:                                                                   ##
## connection - current active DB connection                                    ##
## schema - Schema name to use for SQL Render     - in specified in setings.R   ##
## query - templated MS SQL Server query to execute                             ##
## targetDBMS - query translated to which dialect - in specified in setings.R   ##
##                                                                              ##
##                                                                              ##
## Returns:                                                                     ##
## dataframe with query results                                                 ##
##                                                                              ##
##################################################################################
#' @export
executeSQL <- function (connection, schema, query, targetDBMS) {

    renderedSql <- renderSql(query, cdmSchema=schema)$sql
    translatedSql <- translateSql(renderedSql, sourceDialect = "sql server", targetDialect = targetDBMS)$sql
    if (targetDBMS=="postgresql") {
        queryResults <- dbGetQueryPostgreSql(connection,translatedSql)
    } else {
        queryResults <- dbGetQuery.ffdf(connection,translatedSql)
    }
    return(queryResults)
}

##################################################################################
##  buildKeywordList - This function generates a keyword list based on the      ##
##       expansion of concepts                                                  ##
##                                                                              ##
## connection - current active DB connection                                    ##
## aphrodite_concept_name - concept name to generate keywords off               ##
## schema - Schema name to use for SQL Render translations                      ##
##                                                                              ##
## Returns:                                                                     ##
## status of keyword list (built or not found)                                  ##
##                                                                              ##
##################################################################################
#' @export
buildKeywordList <- function (connection, aphroditeConceptName, schema) {

    concept_of_interest <- executeSQL(connection, schema, paste("SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",aphroditeConceptName,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition';",sep = ""),dbms)

    if (nrow(concept_of_interest) == 0) {
        #No concept under that name found, maybe a typo look in the synonyms table
        concept_of_interest <- executeSQL(connection, schema, paste("SELECT concept_id, concept_synonym_name FROM @cdmSchema.concept_synonym WHERE lower(concept_synonym_name)=lower('",aphroditeConceptName,"');",sep = ""),dbms)
        if (nrow(concept_of_interest) == 0) {
            status <- "No concepts found with the string provided, please try another one."
            return(status)
        }
    }
    #Now that we have our concept (or set of them), we build the lists by expanding them
    keywordlist_clean_df<- list()
    ignorelist_df<- list()

    for (loopC in 1:nrow(concept_of_interest)) {
        currentConcept_id <- concept_of_interest[loopC,1]
        currentConcept_name <- concept_of_interest[loopC,2]
        keywordlist_df <-executeSQL(connection, schema, paste("SELECT ALT.concept_id, ALT.concept_name, ALT.related_concept_id, ALT.related_concept_name FROM ( (SELECT A.concept_id, A.concept_name, B.descendant_concept_id as related_concept_id, C.concept_name as related_concept_name  FROM (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",currentConcept_name,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition') as A, @cdmSchema.concept_ancestor as B, (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE invalid_reason IS NULL AND domain_id = 'Condition') as C WHERE A.concept_id = B.ancestor_concept_id AND C.concept_id = B.descendant_concept_id) UNION (SELECT A.concept_id, A.concept_name, B.ancestor_concept_id as related_concept_id, C.concept_name as related_concept_name  FROM (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",currentConcept_name,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition') as A, @cdmSchema.concept_ancestor as B, (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE invalid_reason IS NULL AND domain_id = 'Condition') as C WHERE A.concept_id = B.descendant_concept_id AND C.concept_id = B.ancestor_concept_id) UNION(SELECT A.concept_id_1 as concept_id, C.concept_name, A.concept_id_2 as related_concept_id, B.concept_name as related_concept_name FROM @cdmSchema.concept_relationship as A, @cdmSchema.concept as B, @cdmSchema.concept as C WHERE concept_id_1 = ",currentConcept_id," AND A.invalid_reason IS NULL AND A.concept_id_2 = B.concept_id AND A.concept_id_1=C.concept_id AND B.domain_id = 'Condition' AND C.domain_id='Condition')) as ALT ORDER BY ALT.related_concept_id;",sep='') ,dbms)
        #Clean the keyword list of the keyword selected
        keywordlist_clean_df[[loopC]]<-subset(keywordlist_df, tolower(c(related_concept_name)) != tolower(c(aphroditeConceptName)))
        #Move the removed terms to the actual ignore file
        ignorelist_df[[loopC]]<-subset(keywordlist_df, tolower(c(related_concept_name)) == tolower(c(aphroditeConceptName)))
    }

    #Compile all ignore lists and keywords lists into one full file each
    ignorelist_ALL <- do.call(rbind, ignorelist_df)
    keywordlist_ALL <- do.call(rbind,keywordlist_clean_df)
    keywordlist_ALL <- keywordlist_ALL[-nrow(keywordlist_ALL),]
    wordLists <- list(keywordlist_ALL = keywordlist_ALL, ignorelist_ALL = ignorelist_ALL)

    return(wordLists)
}

##################################################################################
##  buildKeywordList - This function generates a keyword list based on the      ##
##       expansion of concepts                                                  ##
##                                                                              ##
## connection - current active DB connection                                    ##
## aphrodite_concept_name - concept name to generate keywords off               ##
## schema - Schema name to use for SQL Render translations                      ##
##                                                                              ##
## Returns:                                                                     ##
##                                                                              ##
##################################################################################
#' @export
getdPatientCohort <- function (connection, includeConceptlist, excludeConceptlist, schema, cohortSize, controlSize) {

    #Get empty list
    patients_list_df<- list()
    casesANDcontrols_df<- list()

    #Get all case patients in the cohort - from observations table
    patients_list_df[[1]] <- executeSQL(connection, schema, paste("SELECT distinct(person_id) FROM @cdmSchema.observation WHERE observation_concept_id IN (",paste(includeConceptlist,collapse=","),",",paste(excludeConceptlist,collapse=","),") AND qualifier_concept_id=0;",sep=''),dbms)

    #Get all case patients in the cohort -  from condition occurrence
    patients_list_df[[2]] <- executeSQL(connection, schema, paste("SELECT distinct(person_id) FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(includeConceptlist,collapse=","),",",paste(excludeConceptlist,collapse=","),");",sep=''),dbms)

    #Merge and get unique number of patients - Cases
    casesANDcontrols_df[[1]] <- do.call(rbind, patients_list_df)

    #Get Controls
    #ONLYE GET A REDUCED SET ( NOT IN the full cohort of possible patients)
    casesANDcontrols_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id FROM (SELECT person_id, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM @cdmSchema.person WHERE person_id NOT IN
                                                                     (",paste(as.character(casesANDcontrols_df[[1]]$person_id),collapse=","),")) tmp LIMIT ",controlSize,";" ,sep=''),dbms)

    return(casesANDcontrols_df)
}

##################################################################################
##  getPatientDataCases - We need a separate function for this since it gets    ##
##      the data from the first mention of any of the terms. Only needed for    ##
##      building the model.                                                     ##
##                                                                              ##
## connection - current active DB connection                                    ##
## aphrodite_concept_name - concept name to generate keywords off               ##
## schema - Schema name to use for SQL Render translations                      ##
##                                                                              ##
## Returns:                                                                     ##
##                                                                              ##
##################################################################################
#' @export
getPatientDataCases <- function (connection, patient_ids, keywords, ignores, flags, schema) {
    patientFeatures_drugexposures_df<- list()
    patientFeatures_observations_df<- list()
    patientFeatures_visits_df<- list()
    patientFeatures_labs_df<- list()

    for (patientQueue in 1:(length(patient_ids))) {
        patients_list_df<- list()

        patients_list_df[[1]] <- executeSQL(connection, schema, paste("SELECT person_id, observation_date FROM @cdmSchema.observation WHERE observation_concept_id IN (",paste(keywords,collapse=","),",",paste(ignores,collapse=","),") AND qualifier_concept_id=0 AND person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)

        patients_list_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id, condition_start_date AS observation_date FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(keywords,collapse=","),",",paste(ignores,collapse=","),") AND person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)
        #Find the first date of the term mentions
        dates <- do.call(rbind, patients_list_df)
        remove('patients_list_df')

        #Using the data extract all patient data for the cases
        dateFrom<-min(dates$observation_date)

        if (flags$drugexposures[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_type_concept_id, stop_reason FROM @cdmSchema.drug_exposure WHERE person_id=",as.character(patient_ids[patientQueue])," AND drug_exposure_start_date >='",as.character(dateFrom),"';",sep=''),dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                test1<-aggregate( drug_exposure_id ~ drug_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="drug_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="drug_exposure_id"] <- "counts"
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$observations[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT observation_id, person_id, observation_concept_id, observation_date, observation_type_concept_id FROM @cdmSchema.observation WHERE person_id=",as.character(patient_ids[patientQueue])," AND observation_date>='", as.character(dateFrom),  "' AND qualifier_concept_id = 0;",sep=''), dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                test1<-aggregate( observation_id ~ observation_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="observation_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="observation_id"] <- "counts"
                #change colums to rows
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                #create an empty dataset
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_observations_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')

        }
        if (flags$visits[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id FROM @cdmSchema.visit_occurrence as A, ohdsiv5.condition_occurrence as B WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.visit_start_date >='",as.character(dateFrom), "' AND A.person_id=",as.character(patient_ids[patientQueue]),";",sep=''), dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                test1<-aggregate( condition_occurrence_id ~ condition_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="condition_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="condition_occurrence_id"] <- "counts"
                #change colums to rows
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                #create an empty dataset
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_visits_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$labs[1])  {

            tmp_fv = executeSQL(connection, schema, paste("SELECT measurement_id, person_id, measurement_date, measurement_type_concept_id, value_as_number, value_as_concept_id FROM @cdmSchema.measurement WHERE person_id=",as.character(patient_ids[patientQueue])," AND measurement_date >='",as.character(dateFrom),"';",sep=''), dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                #Remove lab values that did not map properly
                tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="0"),]
                #Remove lab values that have a measurement of NONE
                tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="4124462"),]
                #Create unique categorical categories for lab values
                tmp_fv$type_valueM <- paste(tmp_fv$measurement_type_concept_id, tmp_fv$value_as_concept_id, sep=":")
                test1<-aggregate( measurement_id ~ type_valueM, tmp_fv, function(x) length(unique(x)))

                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                #create an empty dataset
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_labs_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
    }
    dataCases <- list(drugExposures = patientFeatures_drugexposures_df, observations = patientFeatures_observations_df, visits = patientFeatures_visits_df, labs = patientFeatures_labs_df)
    return (dataCases)
}

##################################################################################
##  getPatientData - Generic function to get ALL patient data                   ##
##                                                                              ##
## connection - current active DB connection                                    ##
## aphrodite_concept_name - concept name to generate keywords off               ##
## schema - Schema name to use for SQL Render translations                      ##
##                                                                              ##
## Returns:                                                                     ##
##                                                                              ##
##################################################################################
#' @export
getPatientData <- function (connection, patient_ids, flags, schema) {
    patientFeatures_drugexposures_df<- list()
    patientFeatures_observations_df<- list()
    patientFeatures_visits_df<- list()
    patientFeatures_labs_df<- list()
    for (patientQueue in 1:(length(patient_ids))) {
        if (flags$drugexposures[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_type_concept_id, stop_reason FROM @cdmSchema.drug_exposure WHERE person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                test1<-aggregate( drug_exposure_id ~ drug_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="drug_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="drug_exposure_id"] <- "counts"
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$observations[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT observation_id, person_id, observation_concept_id, observation_date, observation_type_concept_id FROM @cdmSchema.observation WHERE person_id=",as.character(patient_ids[patientQueue])," AND qualifier_concept_id = 0;",sep=''), dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                test1<-aggregate( observation_id ~ observation_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="observation_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="observation_id"] <- "counts"
                #change colums to rows
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                #create an empty dataset
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_observations_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')

        }
        if (flags$visits[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id FROM @cdmSchema.visit_occurrence as A, ohdsiv5.condition_occurrence as B WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.person_id=",as.character(patient_ids[patientQueue]),";",sep=''), dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                test1<-aggregate( condition_occurrence_id ~ condition_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="condition_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="condition_occurrence_id"] <- "counts"
                #change colums to rows
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                #create an empty dataset
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_visits_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$labs[1])  {

            tmp_fv = executeSQL(connection, schema, paste("SELECT measurement_id, person_id, measurement_date, measurement_type_concept_id, value_as_number, value_as_concept_id FROM @cdmSchema.measurement WHERE person_id=",as.character(patient_ids[patientQueue]),";",sep=''), dbms)

            if (nrow(tmp_fv) >0) { #deal with patients with no entries
                #Remove lab values that did not map properly
                tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="0"),]
                #Remove lab values that have a measurement of NONE
                tmp_fv<-tmp_fv[!(tmp_fv$measurement_type_concept_id=="4124462"),]
                #Create unique categorical categories for lab values
                tmp_fv$type_valueM <- paste(tmp_fv$measurement_type_concept_id, tmp_fv$value_as_concept_id, sep=":")
                test1<-aggregate( measurement_id ~ type_valueM, tmp_fv, function(x) length(unique(x)))

                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
                #create an empty dataset
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_labs_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
    }
patientData <- list(drugExposures = patientFeatures_drugexposures_df, observations = patientFeatures_observations_df, visits = patientFeatures_visits_df, labs = patientFeatures_labs_df)
return (patientData)
}

##################################################################################
##  buildFeatureVector - Function to flatten patient data into feature vectors  ##
##                                                                              ##
## Returns:                                                                     ##
##                                                                              ##
##################################################################################
#' @export
buildFeatureVector <- function (flags, casesS, controlsS) {

if (missing(controlsS)) {
    #This only has cases
    featuresDE<-casesS$drugExposures
    featuresOB<-casesS$observations
    featuresVISIT<-casesS$visits

    patientFeatures_cases_labs_df_n<- list()

    if (flags$labs[1]) {
        for (patientQueue in 1:length(casesS$labs)) {
            if (length(casesS$labs[patientQueue])==0) {
                patientFeatures_cases_labs_df_n[[patientQueue]]<-casesS$labs[patientQueue]
            } else {
                tmpJP<-casesS$labs[patientQueue]
                i<- sapply(tmpJP, is.factor)
                tmpJP[i] <- lapply(tmpJP[i], as.character)
                tmp_col <- colnames(tmpJP)
                tmpJP <- as.data.frame(lapply(tmpJP, as.numeric))
                colnames(tmpJP) <- tmp_col
                row.names(tmpJP)<-as.character(row.names(casesS$labs[patientQueue]))
                patientFeatures_cases_labs_df_n[[patientQueue]]<-tmpJP
            }
        }
    }
    featuresLABS<-patientFeatures_cases_labs_df_n
} else {
    featuresDE<-append(casesS$drugExposures,controlsS$drugExposures)
    featuresOB<-append(casesS$observations,controlsS$observations)
    featuresVISIT<-append(casesS$visits,controlsS$visits)
    patientFeatures_controls_labs_df_n<- list()
    patientFeatures_cases_labs_df_n<- list()

    if (flags$labs[1]) {
        for (patientQueue in 1:length(casesS$labs)) {
            if (length(casesS$labs[patientQueue])==0) {
                patientFeatures_cases_labs_df_n[[patientQueue]]<-casesS$labs[patientQueue]
            } else {
                tmpJP<-casesS$labs[patientQueue]
                i<- sapply(tmpJP, is.factor)
                tmpJP[i] <- lapply(tmpJP[i], as.character)
                tmp_col <- colnames(tmpJP)
                tmpJP <- as.data.frame(lapply(tmpJP, as.numeric))
                colnames(tmpJP) <- tmp_col
                row.names(tmpJP)<-as.character(row.names(casesS$labs[patientQueue]))
                patientFeatures_cases_labs_df_n[[patientQueue]]<-tmpJP
            }
        }
    }
    if (flags$labs[1]) {
        for (patientQueue in 1:length(controlsS$labs)) {
            if (length(controlsS$labs[patientQueue])==0) {
                patientFeatures_controls_labs_df_n[[patientQueue]]<-controlsS$labs[patientQueue]
            } else {
                tmpJP<-controlsS$labs[patientQueue]
                i<- sapply(tmpJP, is.factor)
                tmpJP[i] <- lapply(tmpJP[i], as.character)
                tmp_col <- colnames(tmpJP)
                tmpJP <- as.data.frame(lapply(tmpJP, as.numeric))
                colnames(tmpJP) <- tmp_col
                row.names(tmpJP)<-as.character(row.names(controlsS$labs[patientQueue]))
                patientFeatures_controls_labs_df_n[[patientQueue]]<-tmpJP
            }
        }
    }
    featuresLABS<-append(patientFeatures_cases_labs_df_n, patientFeatures_controls_labs_df_n)

}

#We now flatten the vectors
if (flags$observations[1]) {
    test<-featuresOB
    FV_ob <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
    FV_ob <- ddply(FV_ob, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
    colnames(FV_ob)<-paste('obs:',colnames(FV_ob),sep='')
    colnames(FV_ob)[1]<-"pid"
    rm(test)
} else {
    FV_ob <- NULL
}
if (flags$visits[1]) {
    test<-featuresVISIT
    FV_v <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
    FV_v <- ddply(FV_v, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
    colnames(FV_v)<-paste('visit:',colnames(FV_v),sep='')
    colnames(FV_v)[1]<-"pid"
    rm(test)
} else {
    FV_v <- NULL
}
if (flags$drugexposures[1]) {
    test<-featuresDE
    FV_de <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
    FV_de <- ddply(FV_de, .(names), function(x) colSums(x[,-1], na.rm = TRUE))
    colnames(FV_de)<-paste('drugexp:',colnames(FV_de),sep='')
    colnames(FV_de)[1]<-"pid"
    rm(test)
} else {
    FV_de <- NULL
}
if (flags$labs[1]) {
    test<-featuresLABS
    FV_lab <- cbind(names=t(t(c(sapply(test,rownames)))), rbind.fill(test))
    FV_lab[is.na(FV_lab)]<-0
    colnames(FV_lab)<-paste('lab:',colnames(FV_lab),sep='')
    colnames(FV_lab)[1]<-"pid"
    rm(test)
} else {
    FV_lab <- NULL
}

featureVectors <- list(observations = FV_ob, visits = FV_v, labs = FV_lab, observations = FV_ob, drugexposures = FV_de)
return (featureVectors)
}



##################################################################################
##  buildModel - Function to build predictive model for selected patient data   ##
##                                                                              ##
## Returns:                                                                     ##
##                                                                              ##
##################################################################################
#' @export
buildModel <- function (flags, cases_pids, controls_pids, featureVector, outcomeNameS) {
    feature_vectors <- list()

    featuresets=1
    if (flags$drugexposures[1]) {
        feature_vectors[[featuresets]]<-featureVector$drugexposures
        featuresets = featuresets+1
    }
    if (flags$visits[1]) {
        feature_vectors[[featuresets]]<-featureVector$visits
        featuresets = featuresets+1
    }
    if (flags$observations[1]) {
        feature_vectors[[featuresets]]<-featureVector$observations
        featuresets = featuresets+1
    }
    if (flags$labs[1]) {
        feature_vectors[[featuresets]]<-featureVector$labs
        featuresets = featuresets+1
    }

    #Merge all dataframes/Feature vectors for the different sources and have a big list of them
    pp_total = Reduce(function(...) merge(..., by="pid", all=T), feature_vectors)

    #Get selection of FV we wnat
    ppv_set<-pp_total[1:nrow(pp_total),2:ncol(pp_total)]
    #Convert features to boolean or leave as frequency
    if (tolower(c(flags$features_mode[1])) == 'boolean') {
        ppv_set[ppv_set > 0] <- 1  #
    }

    labels <- pp_total$pid %in% cases
    mode(labels) <- "integer"

    replace(labels, labels==0, 'F')
    replace(labels, labels==1, 'T')

    ppv_set[,paste(outcomeNameS)] <- labels

    predictorsNames <- names(ppv_set)[names(ppv_set) != outcomeNameS]

    if (flags$model[1]=='LASSO') {
        ################################################
        # glmnet LASSO                                ##
        ################################################
        # split data into training and testing chunks
        set.seed(567)
        splitIndex <- createDataPartition(ppv_set[,outcomeNameS], p = .75, list = FALSE, times = 1)
        trainDF <- ppv_set[ splitIndex,]
        testDF  <- ppv_set[-splitIndex,]
        # create caret trainControl object to control the number of cross-validations performed
        objControl <- trainControl(method='cv', number=5, returnResamp='none')
        # run model
        objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeNameS], method='glmnet',  metric = "RMSE", trControl=objControl)
        # get predictions on your testing data
        predictions <- predict(object=objModel, testDF[,predictorsNames])
        auc <- roc(testDF[,outcomeNameS], predictions)
        ###### Model Ouputs to file #############
        sink(paste('LASSO output for-',outcomeNameS,'-Cases-',as.character(nCases),'-Controls-',as.character(nControls),'.txt',sep=''))
        cat(paste('Results for LASSO Model for-',outcomeNameS,' using ',as.character(nCases),' Cases and ',as.character(nControls),' Controls. \n\n',sep=''))
        cat("\nModel Summary \n \n")
        # find out variable importance
        print(summary(objModel))
        # find out model details
        cat("\nModel Details \n \n")
        print(objModel)
        cat("\n")
        print(varImp(objModel, scale=F, top=20))
        cat("\nGenerated on ")
        cat(format(Sys.time(), "%a %b %d %Y %X"))
        sink()
    }
    modelReturns <- list(model = objModel, predictors = predictorsNames)
    return (modelReturns)
}


