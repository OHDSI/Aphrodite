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


#' This function executes one single SQL statement
#'
#' @description This function renders, translates and executes one single SQL
#'   statement that produces a result
#'
#' @param connection    The connection to the database server.
#' @param schema        The database schema being used.
#' @param query		    The SQL statement to retrieve the data.
#' @param targetDBMS    The target DBMS for SQL to be rendered in.
#'
#' @details Renders, translates, and executes a single SQL statement that is
#'   expeting to produce and result.
#'
#' @return An object containing the data.
#'
#' @examples \dontrun{
#'
#'   library("SqlRender")
#'   library("DatabaseConnector")
#'   library("Aphordite")
#'   connectionDetails <- createConnectionDetails(dbms="mysql", server="localhost",
#'          user="root", password="blah" ,schema="cdm_v5")
#'   conn <- connect(connectionDetails)
#'
#'   concept_of_interest <- executeSQL(connection, schema, paste("SELECT concept_id,
#'          concept_name FROM @@cdmSchema.concept WHERE lower(concept_name) =
#'          lower('myocardial infarction') AND standard_concept = 'S' AND
#'          invalid_reason IS NULL AND domain_id = 'Condition';" ,sep = ""),dbms)
#'
#'   dbDisconnect(conn)
#' }
#'
#' @export
executeSQL <- function (connection, schema, query, targetDBMS) {

    renderedSql <- renderSql(query, cdmSchema=schema)$sql
    translatedSql <- translateSql(renderedSql, sourceDialect = "sql server", targetDialect = targetDBMS)$sql

    queryResults <- querySql(connection,translatedSql)

    names(queryResults) <- tolower(names(queryResults))  #Hack added to make the field names lowercase - should/might be removed later

    #Broken code from 5/11/2015
    #if (targetDBMS=="postgresql") {
    #    queryResults <- dbGetQueryPostgreSql(connection,translatedSql)
    #} else {
    #    queryResults <- dbGetQuery.ffdf(connection,translatedSql)
    #}
    return(queryResults)
}

#'This function generates keyword and ignore lists based on the expansion of
#'concepts.
#'
#'@description Given any given concept_id or string of text this function
#'generates keyword and ignore lists based on the expansion of concepts (looking
#'at their synonyms).
#'
#'@param connection    The connection to the database server.
#'@param aphroditeConceptName  The string of text / concept name to use.
#'@param schema        The database schema being used.
#'@param dbms          The target DBMS for SQL to be rendered in.
#'
#'@details Takes the aphroditeConceptName looks for synonyms and builds a list
#'of related concepts using the vocabulary hierarchies
#'
#'@return A list with two elements: a list of positive keywords found
#'(keywordlist_ALL), and a list of ignore keywords (ignorelist_ALL)
#'
#' @examples \dontrun{
#'
#'wordLists <- buildKeywordList(conn, aphrodite_concept_name, cdmSchema, dbms)
#'
#' }
#'
#'@export
buildKeywordList <- function (connection, aphroditeConceptName, schema, dbms) {

    concept_of_interest <- executeSQL(connection, schema, paste("SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",aphroditeConceptName,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition';",sep = ""),dbms)

    if (nrow(concept_of_interest) == 0) {
        #No concept under that name found, maybe a typo look in the synonyms table
        concept_of_interest <- executeSQL(connection, schema, paste("SELECT concept_id, concept_synonym_name FROM @cdmSchema.concept_synonym WHERE lower(concept_synonym_name)=lower('",aphroditeConceptName,"');",sep = ""),dbms)
        if (nrow(concept_of_interest) == 0) {
            status <- "No concepts found with the string provided, please try another one."
            stop(status)
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
    wordListsR <- list(keywordlist_ALL = keywordlist_ALL, ignorelist_ALL=ignorelist_ALL)
    return(wordListsR)
}


#'This function builds a patient cohort (and controls) based on a concept list
#'
#'@description This function will build a patient cohort with its respective
#'controls using an inclusion concept_id list as well as an exclussion
#'concept_id list. The user specifies the number of both cases and controls for
#'his cohort.
#'
#'@param connection    The connection to the database server.
#'@param dbms          The target DBMS for SQL to be rendered in.
#'@param includeConceptlist    The list of concept_id's used to build the
#'  cohort.
#'@param excludeConceptlist    The list of concept_id's used as exclusion
#'  criteria for the cohort.
#'@param schema        The database schema being used.
#'@param cohortSize    The number of desired patients to appear in the cohort.
#'@param controlSize   The number of desired patients to be in the control
#'  group.
#'
#'@details This function takes the lists of include and exclude concept_ids and
#'finds all patients that satisfy this characteristics from the Observation and
#'Condition_occurrence tables in CDM V5.
#'
#'@return A list of dataframes containing both cases and control patient_id's.
#'
#' @examples \dontrun{
#'
#'casesANDcontrolspatient_ids_df<- getdPatientCohort(conn, dbms,
#'          as.character(keywordList_FF$V3), as.character(ignoreList_FF$V3),
#'          cdmSchema,nCases,nControls)
#'if (nCases > nrow(casesANDcontrolspatient_ids_df[[1]])) {
#'      message("Not enough patients to get the number of cases specified")
#'      stop
#'} else {
#'    if (nCases > nrow(casesANDcontrolspatient_ids_df[[2]])) {
#'        message("Not enough patients to get the number of controls specified")
#'        stop
#'    }
#'}
#'
#' }
#'
#'@export
getdPatientCohort <- function (connection, dbms, includeConceptlist, excludeConceptlist, schema, cohortSize, controlSize) {

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
    #ONLY GET A REDUCED SET ( NOT IN the full cohort of possible patients)
    casesANDcontrols_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id FROM (SELECT person_id, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM @cdmSchema.person WHERE person_id NOT IN
                                                                     (",paste(as.character(casesANDcontrols_df[[1]]$person_id),collapse=","),")) tmp LIMIT ",controlSize,";" ,sep=''),dbms)

    return(casesANDcontrols_df)
}



#' This function fetches all the patient data (non-generic) designed to work
#' when building a model
#'
#' @description This function fetches all the patient data (non-generic)
#' designed to work when building a model. Returns raw patient data.
#'
#' @param connection    The connection to the database server.
#' @param dbms          The target DBMS for SQL to be rendered in.
#' @param patient_ids   The list of case patient id's to extract data from.
#' @param keywords      The list of concept_id's used to build the cohort.
#' @param ignores       The list of concept_id's ignored when building the
#'   cohort.
#' @param flags         The R dataframe that contains all feature/model flags
#'   specified in settings.R.
#' @param schema        The database schema being used.
#'
#' @details Based on the groups of feature sets determined in the flags
#' variable, this function will fetch patient data. The function determines the
#' first mention of the keywords and selects that date to start the data
#' extraction of the remaining patient information
#'
#' @return An object containing the raw feature sets for the patient data.
#'
#' @examples \dontrun{
#'
#'  dataFcases <-getPatientDataCases(conn, dbms, cases, as.character(keywordList_FF$V3),
#'          flag , cdmSchema)
#'
#' }
#'
#' @export
getPatientDataCases <- function (connection, dbms, patient_ids, keywords, ignores, flags, schema) {
    patientFeatures_drugexposures_df<- list()
    patientFeatures_observations_df<- list()
    patientFeatures_visits_df<- list()
    patientFeatures_labs_df<- list()

    for (patientQueue in 1:(length(patient_ids))) {
        patients_list_df<- list()

        #TODO: change to exclude ignores
        patients_list_df[[1]] <- executeSQL(connection, schema, paste("SELECT person_id, observation_date FROM @cdmSchema.observation WHERE observation_concept_id IN (",paste(keywords,collapse=","),",",paste(ignores,collapse=","),") AND qualifier_concept_id=0 AND person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)

        #TODO: change to exclude ignores
        patients_list_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id, condition_start_date AS observation_date FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(keywords,collapse=","),",",paste(ignores,collapse=","),") AND person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)        #Find the first date of the term mentions
        dates <- do.call(rbind, patients_list_df)
        remove('patients_list_df')

        #Using the data extract all patient data for the cases
        dateFrom<-min(dates$observation_date)

        if (flags$drugexposures[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_type_concept_id, stop_reason FROM @cdmSchema.drug_exposure WHERE person_id=",as.character(patient_ids[patientQueue])," AND drug_exposure_start_date >='",as.character(dateFrom),"';",sep=''),dbms)

            #fv_DF <- getPatientDataCases_Helper(tmp_fv, drug_exposure_id, drug_concept_id, "drug_concept_id", "drug_exposure_id", patient_ids[patientQueue])
            if (nrow(tmp_fv) >0) { 
              #deal with patients with entries
                test1<-aggregate( drug_exposure_id ~ drug_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="drug_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="drug_exposure_id"] <- "counts"
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {  
              #deal with patients with no entries
                test1 <- data.frame(t(data.frame(x = numeric(0))))
            }
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
             rm('test1')
             rm('tmp_fv')
        }
        if (flags$observations[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT observation_id, person_id, observation_concept_id, observation_date, observation_type_concept_id FROM @cdmSchema.observation WHERE person_id=",as.character(patient_ids[patientQueue])," AND observation_date>='", as.character(dateFrom),  "' AND qualifier_concept_id = 0;",sep=''), dbms)

            if (nrow(tmp_fv) >0) { 
              # deal with patients with data
                test1<-aggregate( observation_id ~ observation_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="observation_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="observation_id"] <- "counts"
                #change colums to rows
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
              #deal with patients with no entries
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

            if (nrow(tmp_fv) >0) { 
              # deal with patients with data
                test1<-aggregate( condition_occurrence_id ~ condition_concept_id, tmp_fv, function(x) length(unique(x)))
                names(test1)[names(test1)=="condition_concept_id"] <- "concept_id"
                names(test1)[names(test1)=="condition_occurrence_id"] <- "counts"
                #change colums to rows
                test1<-data.frame(t(test1))
                colnames(test1)[!is.na(test1[1,])] <- test1[1,][!is.na(test1[1,])]
                test1<-test1[-c(1), , drop=FALSE]
            } else {
              #deal with patients with no entries
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
                # TODO: figure this bit out exactly
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

#' This function fetches all the patient data (generic)
#'
#' @description This function fetches all the patient data (generic). Returns
#' raw patient data.
#'
#' @param connection    The connection to the database server.
#' @param dbms          The target DBMS for SQL to be rendered in.
#' @param patient_ids   The list of case patient id's to extract data from.
#' @param flags         The R dataframe that contains all feature/model flags
#'   specified in settings.R.
#' @param schema        The database schema being used.
#'
#' @details Based on the groups of feature sets determined in the flags
#' variable, this function will fetch patient data. The function returns all
#' patient information
#'
#' @return An object containing the raw feature sets for the patient data.
#'
#' @examples \dontrun{
#'
#'  dataFcontrols <- getPatientData(conn, dbms, controls, flag , cdmSchema)
#'
#' }
#'
#' @export
getPatientData <- function (connection, dbms, patient_ids, flags, schema) {
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


#' This function builds a feature vector for a specific subset of features
#'
#' @description This function builds a feature matrix for a specific subset of features, e.g. labs/visits/observations/drug exposures.
#' Returns a feature matrix with all features from all patients included.
#'
#' @param featuresType         A set of patient data in the form of a list of data frames.  Each data frame contains a pid to label the patient, the names of the features that the patient had present, and the frequency counts of these features in his/her record 
#' @param key                  String descriptor of type of feature (e.g. "obs:" or "visit:"). This will be used to label the feature
#' @param labIndic=0           Whether this is for a lab feature.  If so, must be converted from factor to numeric.  Default is 0=no conversion required; 1=conversion required. 
#'
#' @details This function takes a list of patient data frames as input.  Each patient's data frame contains the features that this patient has present in his/her record. This function flattens this information into the combined feature matrix, with all features (of a certain type - e.g. labs or visits) from all patients included.  Clearly, many patients will not have data for many features; their feature counts for any feature that was not present in their record will be set as 0. 
#'
#' @return An data frame of (pts) x (features of input type)
#'
#' @examples \dontrun{
#'
#'  FV_converted<-convertFeatVecPortion(featuresType, 'obs:')
#'  
#'  #OR
#'
#'  FV_converted<-convertFeatVecPortion(featuresType, 'labs:', labIndic=1)
#'
#' }
#'
#' @export
convertFeatVecPortion <- function (featuresType, key, labIndic=0) {
  
  # combine all features into a single data table, with missing features replaced with NA
  featuresType_wNames <- lapply(featuresType, function(x) {x$pid <- rownames(x); x})
  FV_DT <- rbindlist(featuresType_wNames, use.names = TRUE, fill=TRUE)
  
  if (labIndic) {
    # labs are not all in numeric form, so must convert
    FV_DT <- FV_DT[, lapply(.SD, as.numeric), by=pid]
  }
  
  # replace NAs with 0
  FV_DT <- FV_DT[, lapply(.SD, function(x) {x[is.na(x)] <- 0; x}), by=pid]
  FV <- as.data.frame(FV_DT)
  # add description of type of feature (key)
  colnames(FV)<-paste(key,colnames(FV),sep='')
  # in doing this have re-written pid column as key:pid, so change to just pid
  #colnames(FV)[grep(paste(key, "pid"), colnames(FV))]<-"pid"
  colnames(FV)[1]<-'pid'
  return (FV)
}


#' This function builds a feature vector using raw patient data
#'
#' @description This function builds a feature vector using raw patient data.
#' Returns a patient feature vector (divided by feature sets).
#'
#' @param flags         The R dataframe that contains all feature/model flags
#'   specified in settings.R.
#' @param casesS        Dataframe containing the raw patient data.
#' @param controlsS     (OPTIONAL) Dataframe containing the raw patient data.
#'
#' @details This function flattens the patient feature data (per feature set)
#' into a feature vector that will be used as input for caret. This function can
#' optionally flat two sources of patient data (cases and controls)
#'
#' @return An object containing the flattened feature vectors for all given
#' feature sets. Of form: list(observations = FV_ob, visits = FV_v, labs = FV_lab, drugexposures = FV_de)
#'
#' @examples \dontrun{
#'
#'  fv_all<-buildFeatureVector(flag, dataFcases,dataFcontrols)
#'
#'  #OR
#'
#'  fv_cases<-buildFeatureVector(flag, dataFcases)
#'
#' }
#'
#' @export
buildFeatureVector <- function (flags, casesS, controlsS) {

  if (missing(controlsS)) {
      #This only has cases
      featuresDE<-casesS$drugExposures
      featuresOB<-casesS$observations
      featuresVISIT<-casesS$visits
      featuresLABS <- casesS$labs
  }
  else {
    # If cases and controls
      featuresDE<-append(casesS$drugExposures,controlsS$drugExposures)
      featuresOB<-append(casesS$observations,controlsS$observations)
      featuresVISIT<-append(casesS$visits,controlsS$visits)
      featuresLABS <- append(casesS$labs, controlsS$labs)
  }
   
  #We now flatten the vectors
  if (flags$observations[1]) {
      FV_ob <-convertFeatVecPortion(featuresOB, 'obs:')
      message("Obs done")
  } else { 
      FV_ob <- NULL
  }
  
  
  if (flags$visits[1]) {
        FV_v <-convertFeatVecPortion(featuresVISIT, 'visit:')
        message("Visits done")
  } else {
      FV_v <- NULL
  }
  
  
  if (flags$drugexposures[1]) {
        FV_de <-convertFeatVecPortion(featuresDE, 'drugEx:')
        message("Drugs done")
  } else {
      FV_de <- NULL
  }
  
  
  if (flags$labs[1]) {
      FV_lab <- convertFeatVecPortion(featuresLABS, 'lab:', labIndic=1)
      message("Labs done")
  } else {
      FV_lab <- NULL
  }
  
  featureVectors <- list(observations = FV_ob, visits = FV_v, labs = FV_lab, drugexposures = FV_de)
  
  message("Vectors flattened")
return (featureVectors)

}


#' This function combines all of the desired feature types into one single feature vector
#'
#' @description This function combines all of the desired feature types into one single feature vector.  This feature vector is ready to be used for training
#'
#' @param flags         The R dataframe that contains all feature/model flags
#'   specified in settings.R.
#' @param cases_pids    List of patient_id's considered cases (for labeling
#'   purposes)
#' @param controls_pids List of patient_id's considered controls (for labeling
#'   purposes)
#' @param featureVector List of flattened feature vectors returned by buildFeatureVector
#'   function.
#' @param outcomeNameS  String description of the outcome for which the model is
#'   being built
#'
#' @details This function builds a feature vector by concatenating all of the available datasets. 
#' If binary features are specified in the settings, this conversion is made. 
#' The cases_pids and control_pids are patient_id's used for the labeling of
#' the testing and training sets.
#'
#' @return fv_all - The combined feature vector (n patients x n features).  The columns are: pid column, predictorNames, outcomeName
#'
#' @examples \dontrun{
#'
#'  fv_full_data <- combineFeatureVectors(flag, cases, controls, fv_all, outcomeName)
#'
#' }
#'
#' @export
combineFeatureVectors <- function (flags, cases_pids, controls_pids, featureVector, outcomeNameS) {
  # Merge all dataframes/Feature vectors for the different sources and have a big list of them
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
  
  pp_total = Reduce(function(...) merge(..., by="pid", all=T), feature_vectors)
  
  message("Features merged")
  
  # Get class labels based on pids
  cases_pids <- sapply(cases_pids[[1]], function(z) as.character(z))
  controls_pids <- sapply(controls_pids[[1]], function(z) as.character(z))
  labels <- pp_total$pid %in% cases_pids
  
  # Need to rename so that R will be happy with class labels
  labels <- replace(labels, labels==FALSE, 'F')
  labels <-replace(labels, labels==TRUE, 'T')
  
  # Add labels to last column of feature vector
  #[for some reason it doesn't seem to like outcomeNameS here]
  pp_total$Class_labels <- labels
  
  # Get feature names 
  charCols <- c("Class_labels", "pid")
  predictorsNames <- colnames(pp_total)[!colnames(pp_total) %in% charCols]
  
  # Convert to boolean if needed
  if (tolower(c(flags$features_mode[1])) == 'boolean') {
    #TODO write more cleanly
    ppv_set <- pp_total[,predictorsNames]
    ppv_set[ppv_set > 0] <- 1  #
    ppv_set$Class_labels <- labels
    ppv_set$pid <- pp_total$pid
    pp_total <- ppv_set
    message("Features converted to boolean, as set in options")
  }
  else if (tolower(c(flags$features_mode[1])) == 'frequency') {
    message("Features kept as frequencies, as set in options")
  }
  else {
    message("Check options settings for how to define features.  Continuing with default (frequency counts)")
  }
  
  output <- pp_total
  
  return (pp_total)
}



#' This function builds a model for the specified feature vector using cases and
#' controls for a certain outcomeName
#'
#' @description This function builds a model for the specified feature vector
#' using cases and controls for a certain outcomeName. Returns a caret trained
#' model.
#'
#' @param flags         The R dataframe that contains all feature/model flags
#'   specified in settings.R.
#' @param featureVector Flattened feature vector returned by combineFeatureVectors
#'   function, with labeled cases and controls.  Assumed to have one column named "Class_labels" 
#'   and one named "pid"
#' @param outcomeNameS  String description of the outcome for which the model is
#'   being built
#'
#' @details This function builds a model for the specified outcomeName. The
#' model is specified in the flags dataframe (currently only supports LASSO).
#'
#' @return An transferable caret Model object
#'
#' @examples \dontrun{
#'
#'  model_predictors <- buildModel(flag, fv_all, predictorsNames, outcomeName, saveFolder)
#'
#' }
#'
#' @export
buildModel <- function (flags, pp_total, outcomeNameS, saveFolder) {
    
    # Get feature names again 
    charCols <- c("Class_labels", "pid")
    predictorsNames <- colnames(pp_total)[!colnames(pp_total) %in% charCols]
  
    if (flags$model[1]=='LASSO') {
        ################################################
        # glmnet LASSO                                ##
        ################################################
        # split data into training and testing chunks
        set.seed(567)
        splitIndex <- createDataPartition(pp_total$Class_labels, p = .75, list = FALSE, times = 1)
        trainDF <- pp_total[ splitIndex,]
        testDF  <- pp_total[-splitIndex,]
        trainLabels <- pp_total$Class_labels[splitIndex]
        testLabels <- pp_total$Class_labels[-splitIndex]
        # create caret trainControl object to control the number of cross-validations performed
        objControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs=TRUE)
        
        message("Model about to be built")
        
        # run model
        #[RMSE doesn't make sense for classification; must make y a factor]
        objModel <- train(x=trainDF[,predictorsNames], y=factor(trainLabels), method="glmnet",  preProcess=NULL, metric = "Accuracy", trControl=objControl, tuneGrid=NULL)
        
        
        # get predictions on held-out testing data
        # get prediction classes
        predictions <- predict.train(object=objModel, newdata=testDF[,predictorsNames], type='raw')
        modelPerfSummary <- confusionMatrix(predictions, testLabels)
        # get probabilities for each class
        probPreds <- predict(objModel, newdata=testDF[,predictorsNames], type='prob')
        auc <- roc(testLabels, probPreds[,1])
        
        ###### Model Ouputs to file #############
        sink(paste(saveFolder, 'LASSO output for-',outcomeNameS,'-Cases-',as.character(nCases),'-Controls-',as.character(nControls),'.txt',sep=''))
        cat(paste('Results for LASSO Model for-',outcomeNameS,' using ',as.character(nCases),' Cases and ',as.character(nControls),' Controls. \n\n',sep=''))
        cat("\nModel Summary \n \n")
        # find out variable importance
        print(summary(objModel))
        # print model performance
        print(modelPerfSummary)
        # find out model details
        cat("\nModel Details \n \n")
        print(objModel)
        cat("\n")
        print(varImp(objModel, scale=F, top=20))
        cat("\nGenerated on ")
        cat(format(Sys.time(), "%a %b %d %Y %X"))
        sink()
    }
    modelReturns <- list(model = objModel, predictors = predictorsNames, auc)
    return (modelReturns)
}





#' This function returns the concept terms corresponding to an input set of concept
#' IDs.  
#'
#' @description This function returns the concept terms corresponding to an input set of concept
#' IDs.  
#'
#' @param connection    The connection to the database server.
#' @param schema        The database schema being used
#' @param dbms          The target DBMS for SQL to be rendered in.
#' @param model         The model object; will be used to extract top-ranking features
#' @param numFeats      The number of features you'd like returned
#'
#' @details This function returns the concept terms corresponding to an input set of concept
#' IDs.  Use case: to investigate highly-ranked features from classification model
#'
#' @return A list of concept terms and concept ids, corresponding to the IDs of interest
#'
#' @examples \dontrun{
#'
#'  high_ranking_concepts <- conceptDecoder(connection, schema, dbms, model, 20)
#'
#' }
#'
#' @export
conceptDecoder <- function (connection, schema, dbms, model, numFeats) {
  
  # get model rankings
  modelRankDetails <- varImp(model, scale=F, top=20)
  featImps <- modelRankDetails$importance
  ids <-row.names(featImps)

  # put data into df
  featImpDF <- data.frame(type=sapply(ids, function(x) unlist(strsplit(x, ":"))[1]), ids=sapply(ids, function(x) unlist(strsplit(x, ":"))[2]), importance=featImps$Overall, absImportance=abs(featImps$Overall))
  
  # sort by abs value
  featImpDF <- featImpDF[with(featImpDF, order(-absImportance)), ]
  
  # return selection
  selection <- featImpDF[1:numFeats,]
  selection$rank <- c(1:numFeats)
  
  # make sql query
  #concepts <- sapply(featCodesN, function(x) executeSQL(connection, schema, paste("SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE concept_id=",x,";", sep=""),dbms))  # for debugging to check ids are correct
  concepts <- sapply(selection$ids, function(x) executeSQL(connection, schema, paste("SELECT concept_name FROM @cdmSchema.concept WHERE concept_id=",x,";", sep=""),dbms))
  
  # add concepts to output
  selection$concepts <- as.character(concepts)
  
  return (selection)
}







#' This function returns the predictions for a set of gold-standard patients, based on the trained model
#'
#' @description This function returns the predictions for a set of gold-standard patients, based on the trained model
#'
#' @param connection    The connection to the database server.
#' @param schema        The database schema being used
#' @param dbms          The target DBMS for SQL to be rendered in.
#' @param model         The trained model object; will be used to predict for new patients
#' @param pids          The list of gold-standard patients to evaluate
#' @param flag          Set of flags from settings.R file; determines which feature sets to include
#'
#' @details This function returned predicted classes for the input patient list.  Use case: evaluate trained model on a set of gold-standard patients.  
#'
#' @return Predicted classes and their probabilities, for each input patient ID
#'
#' @examples \dontrun{
#'
#'  gs_predictions <- testModel(connection, schema, dbms, model, pids, flag)
#'
#' }
#'
#' @export
testModel <- function(conn, schema, dbms, model, pids, flag) {
  #TODO This function is still bug-y!
  
  # get data for all patients
  dataFtests <- getPatientData(conn, dbms, pids, flag, schema)
  
  # get feature vector
  fv_all<-buildFeatureVector(flag, dataFtests)
  
  # get prediction probabilities
  probPreds <- predict(model, newdata=testDF[,predictorsNames], type='prob')
  
  return(probPreds)
  
}





#' This function plots the feature importance weightings
#'
#' @description This function plots the feature importance weightings
#'
#' @param plotSaveFile        The name of the file to save
#' @param weightingsDF        Data frame of the weightings with their labels
#'
#' @details This function returned predicted classes for the input patient list.  Use case: evaluate trained model on a set of gold-standard patients.  
#'
#' @return (none)
#'
#' @examples \dontrun{
#'
#'  plotFeatWeightings(plotSaveFile, weightingsDF)
#'
#' }
#'
#' @export
plotFeatWeightings <- function (plotSaveFile, weightingsDF) {
  
  # plot
  labels.wrap  <- lapply(strwrap(weightingsDF$concept,50,simplify=F),paste,collapse="\n") # word wrap
  g<-ggplot(weightingsDF, aes(rank, importance))+
    geom_point(color='firebrick') + 
    # TODO: why is bar (below) not working?
    #geom_bar(stat='identity', color="firebrick")+
    labs(x='', y="Feature Importance" , title=paste("Feature Importance for",studyName)) +
    scale_x_discrete(labels=labels.wrap) +
    theme(axis.text.x = element_text(labels.wrap, size=3, angle=90)) +
    #theme(axis.text.y = element_text(size=10))
    coord_flip()
  g
  ggsave(plotSaveFile, width = 16, height = 9, dpi = 120)

}




