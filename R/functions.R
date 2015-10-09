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
    ### JMB: Added nasty hack to remove semi colon for Oracle instances ##
    ### Will probably remove after more testing - with sending no semi-colons on PostGre ##
    if (tolower(c(targetDBMS))=="oracle") {
        query= substr(query,1,nchar(query)-1)
    } #Not oracle? do nothing
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
        keywordlist_df <-executeSQL(connection, schema, paste("SELECT ALT.concept_id, ALT.concept_name, ALT.related_concept_id, ALT.related_concept_name FROM ( (SELECT A.concept_id, A.concept_name, B.descendant_concept_id as related_concept_id, C.concept_name as related_concept_name  FROM (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",currentConcept_name,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition') A, @cdmSchema.concept_ancestor B, (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE invalid_reason IS NULL AND domain_id = 'Condition') C WHERE A.concept_id = B.ancestor_concept_id AND C.concept_id = B.descendant_concept_id) UNION (SELECT A.concept_id_1 as concept_id, C.concept_name, A.concept_id_2 as related_concept_id, B.concept_name as related_concept_name FROM @cdmSchema.concept_relationship A, @cdmSchema.concept B, @cdmSchema.concept C WHERE concept_id_1 = ",currentConcept_id," AND A.invalid_reason IS NULL AND A.concept_id_2 = B.concept_id AND A.concept_id_1=C.concept_id AND B.domain_id = 'Condition' AND C.domain_id='Condition')) ALT ORDER BY ALT.related_concept_id;",sep='') ,dbms)
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

    #Get all case patients in the cohort - from observations table - remove patients with ignore keywords
    #patients_list_df[[1]] <- executeSQL(connection, schema, paste("SELECT distinct(person_id) FROM @cdmSchema.observation WHERE observation_concept_id IN (",paste(includeConceptlist,collapse=","),",", paste(excludeConceptlist,collapse=","), ") AND qualifier_concept_id=0;",sep=''),dbms)
    patients_list_df[[1]] <- executeSQL(connection, schema, paste("SELECT distinct(person_id) FROM @cdmSchema.observation WHERE observation_concept_id IN (", paste(includeConceptlist,collapse=","), ") AND observation_concept_id NOT IN (", paste(excludeConceptlist,collapse=","), ") AND qualifier_concept_id=0;",sep=''),dbms)

    #Get all case patients in the cohort -  from condition occurrence - remove patients with ignore keywords
    #patients_list_df[[2]] <- executeSQL(connection, schema, paste("SELECT distinct(person_id) FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(includeConceptlist,collapse=","),",",paste(excludeConceptlist,collapse=","),");",sep=''),dbms)
    patients_list_df[[2]] <- executeSQL(connection, schema, paste("SELECT distinct(person_id) FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(includeConceptlist,collapse=","), ") AND condition_concept_id NOT IN (", paste(excludeConceptlist,collapse=","), ");", sep=''),dbms)

    #Merge and get unique number of patients - Cases
    casesANDcontrols_df[[1]] <- do.call(rbind, patients_list_df)

    #Get Controls
    #This gets the reduced set with no need for a limit statement
    #if (tolower(c(dbms))=="oracle") {
    #    casesANDcontrols_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id FROM (SELECT person_id, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM @cdmSchema.person WHERE person_id NOT IN (",paste(as.character(casesANDcontrols_df[[1]]$person_id),collapse=","),")) tmp WHERE rn <= ",controlSize,";" ,sep=''),dbms)
    #} else {
    casesANDcontrols_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id FROM (SELECT TM.person_id, ROW_NUMBER() OVER (ORDER BY RAND()) AS rn FROM (SELECT A.person_id FROM @cdmSchema.person A LEFT JOIN ( (SELECT distinct(person_id) FROM @cdmSchema.observation WHERE observation_concept_id NOT IN (", paste(excludeConceptlist,collapse=","), ") AND observation_concept_id IN (", paste(includeConceptlist,collapse=","), ") AND qualifier_concept_id=0)  UNION (SELECT distinct(person_id) FROM @cdmSchema.condition_occurrence WHERE condition_concept_id NOT IN (", paste(excludeConceptlist,collapse=","), ") AND condition_concept_id IN (", paste(includeConceptlist,collapse=","), "))) B ON A.person_id=B.person_id WHERE B.person_id IS NULL) TM) tmp WHERE rn <= ",controlSize,";" ,sep=''),dbms)
    #}


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
#' @param removeDomains=''   List of domains to not include as features, if any are specified in settings file
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
getPatientDataCases <- function (connection, dbms, patient_ids, keywords, ignores, flags, schema, removeDomains='') {
    patientFeatures_drugexposures_df<- list()
    patientFeatures_observations_df<- list()
    patientFeatures_visits_df<- list()
    patientFeatures_labs_df<- list()

    #removeDomains <- flags$remove_domains[1]

    for (patientQueue in 1:(length(patient_ids))) {
        patients_list_df<- list()

        #NOW: just looks at keywords.
        patients_list_df[[1]] <- executeSQL(connection, schema, paste("SELECT person_id, observation_date FROM @cdmSchema.observation WHERE observation_concept_id IN (", paste(keywords,collapse=","), ") AND qualifier_concept_id=0 AND person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)

        #NOW:just looks at keywords.
        patients_list_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id, condition_start_date AS observation_date FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (",paste(keywords,collapse=","),") AND person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)        #Find the first date of the term mentions
        dates <- do.call(rbind, patients_list_df)
        remove('patients_list_df')

        # Set range for data extract
        if (flags$timeWindowOpt[1]==1) {
            # if want to go from first term appearance in notes
            dateStart<-min(dates$observation_date)
            dateEnd<- max(dates$observation_date)
        } else if (flags$timeWindowOpt[1]==2) {
          # if want to go from 10 years prior to first term appearance in notes
            dateEnd <- min(dates$observation_date)
            dateStart <- as.POSIXlt(dateEnd)
            dateStart$year <- dateStart$year - 10
            dateStart <- as.Date(dateStart)
        }

        # get normalization term
        timeDiff <- getNormalizationTerm(dates, flags)

        #Using the data extract all patient data for the cases

        if (flags$drugexposures[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT A.drug_exposure_id, A.person_id, A.drug_concept_id as concept_id, A.drug_exposure_start_date as feat_date, A.drug_type_concept_id, A.stop_reason, B.concept_name FROM @cdmSchema.drug_exposure A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.drug_exposure_start_date >='",as.character(dateStart),"' AND A.drug_exposure_start_date <='",as.character(dateEnd), "' AND A.drug_concept_id=B.concept_id AND B.standard_concept='S' AND B.invalid_reason IS NULL AND B.domain_id NOT IN (", paste(removeDomains,collapse=","), ") AND B.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$observations[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT A.observation_id, A.person_id, A.observation_concept_id as concept_id, A.observation_date as feat_date, A.observation_type_concept_id, B.concept_name, B.domain_id FROM @cdmSchema.observation A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.observation_date>='", as.character(dateStart),  "' AND A.observation_date<='", as.character(dateEnd),  "' AND A.qualifier_concept_id = 0 AND A.observation_concept_id=B.concept_id AND B.standard_concept='S' AND B.invalid_reason IS NULL AND B.domain_id NOT IN (", paste(removeDomains,collapse=","), ") AND B.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_observations_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$visits[1]) {

            tmp_fv = executeSQL(connection, schema, paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date as feat_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id as concept_id, C.concept_name FROM @cdmSchema.visit_occurrence A, @cdmSchema.condition_occurrence B, @cdmSchema.concept C WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.visit_start_date >='",as.character(dateStart), "'AND A.visit_start_date <='",as.character(dateEnd), "' AND A.person_id=",as.character(patient_ids[patientQueue])," AND B.condition_concept_id=C.concept_id AND C.standard_concept='S' AND C.invalid_reason IS NULL AND C.domain_id NOT IN (", paste(removeDomains,collapse=","), ") AND C.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_visits_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$labs[1])  {

          tmp_fv = executeSQL(connection, schema, paste("SELECT A.measurement_id, A.person_id, A.measurement_date, A.measurement_type_concept_id, A.measurement_concept_id, A.value_as_number, A.value_as_concept_id, B.concept_name FROM @cdmSchema.measurement A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.measurement_id NOT IN (", paste(keywords,collapse=","), ") AND A.measurement_concept_id=B.concept_id AND A.measurement_date >='",as.character(dateStart),"' AND A.measurement_date <='",as.character(dateEnd),"' AND A.measurement_id NOT IN (", paste(keywords,collapse=","), ") AND A.measurement_concept_id!=0 AND A.measurement_concept_id!=4124462;", sep=''), dbms)
          tmp_fv$concept_id <- paste(tmp_fv$measurement_concept_id, tmp_fv$value_as_concept_id, sep=":")
          test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)

          #COMMENTS RELEVANT TO OLD CODE:
            # safety catches - now addressed within sql query
            #Remove lab values that did not map properly
            #0 is concept code for "no matching value" - removes these
            # [changed these to measurement_concept_id rather than measurement_type_concept_id - this seems to make more sense]
            #uncomment if remove from sql query: tmp_fv<-tmp_fv[!(tmp_fv$measurement_concept_id=="0"),]
            #Remove lab values that have a measurement of NONE
            #4124462 is concept_code for "qualifier value" - removes these
            #uncomment if remove from sql query: tmp_fv<-tmp_fv[!(tmp_fv$measurement_concept_id=="4124462"),]
            #Create unique categorical categories for lab values
            #[changed to measurement_concept_id rather than measurement_type_concept_id - this seems to make more sense - old: tmp_fv$type_valueM <- paste(tmp_fv$measurement_type_concept_id, tmp_fv$value_as_concept_id, sep=":")]
          row.names(test1)<-as.character(patient_ids[patientQueue])
          patientFeatures_labs_df[[patientQueue]]<-test1
          rm('test1')
          rm('tmp_fv')
        }
    }
    dataCases <- list(drugExposures = patientFeatures_drugexposures_df, observations = patientFeatures_observations_df, visits = patientFeatures_visits_df, labs = patientFeatures_labs_df)
    return (dataCases)
}





#' This function performs the manipulation of the sql extract data; should be generic for any of the feature types
#'
#' @description This function performs the manipulation of the sql extract data; should be generic for any of the feature types
#'
#' @param tmp_fv          Pull from sql query.  Should have a column for date and concept_id
#' @param flags          Flags set in settings - specifies which normalization is needed
#' @param timeDiff        Value to use for normalization
#'
#' @details This is just a helper function that reduces the repeats of code for the manipulation of the sql extract data, so that it is put in the desired format for compiling all patient features together.  This function: gets the counts of codes on a given visit (so multiple codes/terms/drugs/etc are not all counted); normalizes based on the normalization setting; returns a data frame with counts of codes
#'
#' @return An object containing the re-formatted patient data: ptID x (num concept IDs) - filled with counts, deduplicated by visit
#'
#' @examples \dontrun{
#'
#'  test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
#'
#' }
#'
#' @export
manipulateSqlPull <- function(tmp_fv, flags, timeDiff) {

      if (nrow(tmp_fv) >0) {
          # I don't think this takes into account multiple terms on the same date:
          # test1<-aggregate( drug_exposure_id ~ drug_concept_id, tmp_fv, function(x) length(unique(x)))
          # replace with:
          ptData <- as.data.table(tmp_fv)
          byDate <- dcast.data.table(ptData, feat_date ~ concept_id, fun=function(x) {if (length(x)>0) {1} else {0}}, value.var='concept_id')  # counts each code just once per visit
          #byDate2 <- dcast.data.table(ptData, drug_exposure_start_date ~ drug_concept_id, fun=length)  # if you want to keep the counts per visit
          byDate <- as.data.frame(byDate)

          # get sums of counts across dates (counts are deduplicated by date)
          if (ncol(byDate)>2) {
              byDateSum <- as.data.frame(colSums(byDate[,colnames(byDate)!='feat_date']))
          }
          else {  #colsums doesn't work if there is only 1 column (2 above in if statement b/c also column for date, which is ignored in sum)
              byDateSum <- as.data.frame(sum(byDate[,colnames(byDate)!='feat_date']))
              rownames(byDateSum) <- colnames(byDate)[colnames(byDate)!='feat_date']
          }
          colnames(byDateSum) <- c("counts")
          byDateSum$concept_id <- rownames(byDateSum)

          # normalize data
          if (flags$timeNormalize[1]==4) {
              # if normalizing by the number of measurements
              timeDiff <- sum(byDateSum$counts)
          } else if (flags$timeNormalize[1]==5) {
              # if normalizing by the number of unique measurements
              timeDiff <- nrow(byDateSum)
          }
          # normalize [timeDiff already defined if alternative normalization options]
          byDateSum$counts <- byDateSum$counts/timeDiff

          test1 <- data.frame(t(byDateSum$counts))
          colnames(test1) <- byDateSum$concept_id

      } else {
        # If no data
            test1 <- data.frame(t(data.frame(x = numeric(0))))
      }

      return (test1)
}



#' This function returns the normalizing factor, based upon the input settings
#'
#' @description This function returns the normalizing factor, based upon the input settings
#'
#' @param dates    The dates of all visits recorded in the record
#' @param flags         The R dataframe that contains all feature/model flags
#'   specified in settings.R. - specifies which sort of normalization to perform
#' @param defaultTime   Value by which to normalize patients who only have a single visit, so cannot say what the follow-up time is (=0 --> undefined).  Set default time as 1; as if spreading single observation over an entire year or 1 month (depending on settings)
#'
#' @details Depending upon the input settings, will return the normalizing term for the feature values.  Some normalization settings depend upon the specific feature type; these are addressed within the individual feature types.  This is a helper function for getPatientData
#'
#' @return The value by which to divide term counts
#'
#' @examples \dontrun{
#'
#'  timeDiff <- getNormalizationTerm(dates, flags)
#'
#' }
#'
#' @export
getNormalizationTerm <- function (dates, flags, defaultTime=1) {
      # create normalizing term
      # defaultTime = time to put for patients who don't have any follow-up time
      if (flags$timeNormalize[1]==1) {
          # if normalizing by length of follow-up in years
          if (length(dates$observation_date)<1) {
              timeDiff <- defaultTime
          } else {
              # time diff in years
              timeDiff <- (as.numeric(max(dates$observation_date) - min(dates$observation_date)))/365
          }

      } else if (flags$timeNormalize[1]==2) {
          # if normalizing by length of follow-up in months
          if (length(dates$observation_date)<1) {
              timeDiff <- defaultTime
          } else {
              # time diff in months
              timeDiff <- ((as.numeric(max(dates$observation_date) - min(dates$observation_date)))/365)*12
          }

      } else if (flags$timeNormalize[1]==3) {
          # if normalizing by the number of visits
          timeDiff <- nrow(unique(dates))

      } else {
          # this includes both flags$timeNormalize[1]==0 --> no normalization AND flags$timeNormalize[1]==4 --> normalize by the number of measurements in the category (addressed within sections)
          # divide counts by 1 --> no normalization occurs
          timeDiff <- 1
      }

      # adjust if no time difference
      if (timeDiff==0) {
          timeDiff <- defaultTime
      }

      return (timeDiff)
}



#' This function fetches all the patient data (generic)
#'
#' @description This function fetches all the patient data (generic). Returns
#' raw patient data.
#'
#' @param connection    The connection to the database server.
#' @param dbms          The target DBMS for SQL to be rendered in.
#' @param patient_ids   The list of case patient id's to extract data from - NOT a data.frame.
#' @param keywords      The list of concept_id's that are NOT wanted to be used as features
#' @param flags         The R dataframe that contains all feature/model flags
#'   specified in settings.R.
#' @param schema        The database schema being used.
#' @param removeDomains=''   List of domains to not include as features, if any are specified in settings file
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
getPatientData <- function (connection, dbms, patient_ids, keywords, flags, schema, removeDomains=c('')) {
    patientFeatures_drugexposures_df<- list()
    patientFeatures_observations_df<- list()
    patientFeatures_visits_df<- list()
    patientFeatures_labs_df<- list()

    # domains that we do not want to include as features
    #removeDomains <- flags$remove_domains[1]

    for (patientQueue in 1:(length(patient_ids))) {

        patients_list_df<- list()

        # get patient dates
        patients_list_df[[1]] <- executeSQL(connection, schema, paste("SELECT person_id, observation_date FROM @cdmSchema.observation WHERE qualifier_concept_id=0 AND person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)
        patients_list_df[[2]] <- executeSQL(connection, schema, paste("SELECT person_id, condition_start_date AS observation_date FROM @cdmSchema.condition_occurrence WHERE person_id=",as.character(patient_ids[patientQueue]),";",sep=''),dbms)
        dates <- do.call(rbind, patients_list_df)
        remove('patients_list_df')

        # get normalization term
        timeDiff <- getNormalizationTerm(dates, flags)

        if (flags$drugexposures[1]) {
            if (removeDomains=='') { #No need to filter by domains if not present
                tmp_fv = executeSQL(connection, schema, paste("SELECT A.drug_exposure_id, A.person_id, A.drug_concept_id as concept_id, A.drug_exposure_start_date as feat_date, A.drug_type_concept_id, A.stop_reason, B.concept_name FROM @cdmSchema.drug_exposure A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.drug_concept_id=B.concept_id AND B.standard_concept='S' AND B.invalid_reason IS NULL AND B.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            } else {
                tmp_fv = executeSQL(connection, schema, paste("SELECT A.drug_exposure_id, A.person_id, A.drug_concept_id as concept_id, A.drug_exposure_start_date as feat_date, A.drug_type_concept_id, A.stop_reason, B.concept_name FROM @cdmSchema.drug_exposure A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.drug_concept_id=B.concept_id AND B.standard_concept='S' AND B.invalid_reason IS NULL  AND B.domain_id NOT IN (", paste(removeDomains,collapse=","), ") AND B.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            }
            test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_drugexposures_df[[patientQueue]]<-test1   #Assign the already transformed FV
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$observations[1]) {
            if (removeDomains=='') { #No need to filter by domains if not present
                tmp_fv = executeSQL(connection, schema, paste("SELECT A.observation_id, A.person_id, A.observation_concept_id as concept_id, A.observation_date as feat_date, A.observation_type_concept_id, B.concept_name, B.domain_id FROM @cdmSchema.observation A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.qualifier_concept_id = 0 AND A.observation_concept_id=B.concept_id AND B.standard_concept='S' AND B.invalid_reason IS NULL AND B.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            } else {
                tmp_fv = executeSQL(connection, schema, paste("SELECT A.observation_id, A.person_id, A.observation_concept_id as concept_id, A.observation_date as feat_date, A.observation_type_concept_id, B.concept_name, B.domain_id FROM @cdmSchema.observation A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.qualifier_concept_id = 0 AND A.observation_concept_id=B.concept_id AND B.standard_concept='S' AND B.invalid_reason IS NULL AND B.domain_id NOT IN (", paste(removeDomains,collapse=","), ") AND B.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            }
            test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_observations_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')

        }
        if (flags$visits[1]) {
            if (removeDomains=='') { #No need to filter by domains if not present
                tmp_fv = executeSQL(connection, schema, paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date as feat_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id as concept_id, C.concept_name FROM @cdmSchema.visit_occurrence A, ohdsiv5.condition_occurrence B, @cdmSchema.concept C WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.person_id=",as.character(patient_ids[patientQueue])," AND B.condition_concept_id=C.concept_id AND C.standard_concept='S' AND C.invalid_reason IS NULL  AND C.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            } else {
                tmp_fv = executeSQL(connection, schema, paste("SELECT A.visit_occurrence_id, A.person_id, A.visit_start_date as feat_date, A.visit_end_date, B.condition_occurrence_id, B.condition_concept_id as concept_id, C.concept_name FROM @cdmSchema.visit_occurrence A, ohdsiv5.condition_occurrence B, @cdmSchema.concept C WHERE A.visit_occurrence_id = B.visit_occurrence_id AND A.person_id=",as.character(patient_ids[patientQueue])," AND B.condition_concept_id=C.concept_id AND C.standard_concept='S' AND C.invalid_reason IS NULL  AND C.domain_id NOT IN (", paste(removeDomains,collapse=","), ") AND C.concept_id NOT IN (", paste(keywords,collapse=","), ");",sep=''), dbms)
            }
            test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_visits_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
        if (flags$labs[1])  {
            tmp_fv = executeSQL(connection, schema, paste("SELECT A.measurement_id, A.person_id, A.measurement_date as feat_date, A.measurement_type_concept_id, A.measurement_concept_id, A.value_as_number, A.value_as_concept_id, B.concept_name FROM @cdmSchema.measurement A, @cdmSchema.concept B WHERE A.person_id=",as.character(patient_ids[patientQueue])," AND A.measurement_id NOT IN (", paste(keywords,collapse=","), ") AND A.measurement_concept_id=B.concept_id AND A.measurement_id NOT IN (", paste(keywords,collapse=","), ") AND A.measurement_concept_id!=0 AND A.measurement_concept_id!=4124462;", sep=''), dbms)
            tmp_fv$concept_id <- paste(tmp_fv$measurement_concept_id, tmp_fv$value_as_concept_id, sep=":")
            test1 <- manipulateSqlPull(tmp_fv, flags, timeDiff)
            row.names(test1)<-as.character(patient_ids[patientQueue])
            patientFeatures_labs_df[[patientQueue]]<-test1
            rm('test1')
            rm('tmp_fv')
        }
      #message(patientQueue)
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
convertFeatVecPortion <- function (featuresType, key, labIndic) {

  # combine all features into a single data table, with missing features replaced with NA
  featuresType_wNames <- lapply(featuresType, function(x) {x$pid <- rownames(x); x})
  FV_DT <- rbindlist(featuresType_wNames, use.names = TRUE, fill=TRUE)

  #if (labIndic) {
    # labs are not all in numeric form, so must convert
    FV_DT <- FV_DT[, lapply(.SD, as.numeric), by=pid]
  #}

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
      FV_ob <-convertFeatVecPortion(featuresOB, 'obs:',1)
      message("STATUS: Observations done")
  } else {
      FV_ob <- NULL
  }

  if (flags$visits[1]) {
        FV_v <-convertFeatVecPortion(featuresVISIT, 'visit:',1)
        message("STATUS: Visits done")
  } else {
      FV_v <- NULL
  }

  if (flags$drugexposures[1]) {
        FV_de <-convertFeatVecPortion(featuresDE, 'drugEx:',1)
        message("STATUS: Drugs done")
  } else {
      FV_de <- NULL
  }

  if (flags$labs[1]) {
      FV_lab <- convertFeatVecPortion(featuresLABS, 'lab:', 1)
      message("STATUS: Measurements done")
  } else {
      FV_lab <- NULL
  }

  featureVectors <- list(observations = FV_ob, visits = FV_v, labs = FV_lab, drugexposures = FV_de)
  message("STATUS: Feature vectors are ready")
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
#'   being built [Not actually needed]
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

  message("STATUS: All features merged")

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
    message("STATUS: Features converted to boolean, as set in options")
  }
  else if (tolower(c(flags$features_mode[1])) == 'frequency') {
    message("STATUS: Features kept as frequencies, as set in options")
  }
  else {
    message("STATUS: Check options settings for how to define features.  Continuing with default (frequency counts)")
  }


  # Filter to remove features found in less than 2% of patients
  ppv_bin <- pp_total[,predictorsNames]  # get numeric data
  ppv_bin[ppv_bin > 0] <- 1  # binarize
  colSums <- colSums(ppv_bin) # get column sums
  cutoff <- flags$threshCutoff[1]*nrow(ppv_bin)  # find number of patients who need to have feature to keep it
  keepRows <- predictorsNames[colSums>cutoff]  # get new list of features
  # re-assign
  pp_final <- pp_total[,keepRows]
  pp_final$Class_labels <- pp_total$Class_labels
  pp_final$pid <- pp_total$pid


  return (pp_final)
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
#' @param saveFolder   folder in which summary file output will be saved
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

    ################################################
    # build model                                ##
    ################################################
    # split data into training and testing chunks
    #set.seed(567)
    splitIndex <- createDataPartition(pp_total$Class_labels, p = .75, list = FALSE, times = 1)
    trainDF <- pp_total[splitIndex,]
    testDF  <- pp_total[-splitIndex,]
    trainLabels <- pp_total$Class_labels[splitIndex]
    testLabels <- pp_total$Class_labels[-splitIndex]
    # create caret trainControl object to control the number of cross-validations performed
    #objControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs=TRUE, summaryFunction=twoClassSummary) - if want to use ROC for metric
    objControl <- trainControl(method='cv', number=5, returnResamp='none', classProbs=TRUE, summaryFunction=f_score_calc)

    message("Model about to be built")
    if (flags$model[1]=='LASSO') {
        # set parameter grid
        lr_grid <- expand.grid(alpha = seq(0,1,length=10), lambda = seq(0.001, 2, length=10))
        # run lasso LR model
        #TODO: find out when preprocessing happens in the code (ie, just on training set during cv, or applied to all the data? preProcess=c("center", "scale") ) - I think applied to all data
      objModel <- train(x=trainDF[,predictorsNames], y=factor(trainLabels), method="glmnet", metric = "Fscore", trControl=objControl)#, tuneGrid=lr_grid)
    } else if (flags$model[1]=='RF') {
        # run random forest model
        rf_grid <- expand.grid(mtry=round(seq(.1*length(predictorsNames), .9*length(predictorsNames), length=6)))
        objModel <- train(x=trainDF[,predictorsNames], y=factor(trainLabels), method="rf",metric = "Fscore", trControl=objControl, tuneGrid=rf_grid, preProcess=c("center", "scale"))
    }

    # get predictions on held-out testing data
    # get prediction classes
    predictions <- predict.train(object=objModel, newdata=testDF[,predictorsNames], type='raw')
    modelPerfSummary <- confusionMatrix(predictions, testLabels, positive='T')
    # get probabilities for each class
    probPreds <- predict(objModel, newdata=testDF[,predictorsNames], type='prob')
    auc <- roc(testLabels, probPreds[,1])

    ###### Model Ouputs to file #############
    #sink(paste(saveFolder, flags$model[1], ' output for-',outcomeNameS,'-Cases-',as.character(nCases),'-Controls-',as.character(nControls),'.txt',sep=''))
    sink(paste(saveFolder, flags$model[1], '_output_',outcomeNameS,'.txt',sep=''))
    #cat(paste('Results for ',  flags$model[1], 'Model for-',outcomeNameS,' using ',as.character(nCases),' Cases and ',as.character(nControls),' Controls. \n\n',sep=''))
    cat(paste('Results for ', flags$model[1], ' Model for-',outcomeNameS,' using ',as.character(nControls),' Controls. \n\n',sep=''))
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

    modelReturns <- list(model = objModel, predictorsNames = predictorsNames, auc=auc, testSet=testDF, testProbs = probPreds)
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
#' @param breaker=":"       Which sort of breaker is used in feature names (e.g. for "obs:12345" it would be ":")
#' @param typeInd1=1      Indice after string split defining which feature class (e.g. [1] for "obs:12345" defines obs)
#' @param idInd=1       Indice after string split defining which concept_id (e.g. [2] for "obs:12345
#'  defines "12345")
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
conceptDecoder <- function (connection, schema, dbms, model, numFeats, breaker=':', typeInd=1, idInd=2) {

      # get model rankings
      modelRankDetails <- varImp(model, scale=F)  # if leave scale as true, give feature weightings scaled from 0-100 (we want to keep sign of weightings)
      featImps <- modelRankDetails$importance
      ids <-row.names(featImps)

      # put data into df
      #TODO address different labeling for labs
      featImpDF <- data.frame(type=sapply(ids, function(x) unlist(strsplit(x, breaker))[typeInd]), ids=sapply(ids, function(x) unlist(strsplit(x, breaker))[idInd]), importance=featImps$Overall, absImportance=abs(featImps$Overall))

      # sort by abs value
      featImpDF <- featImpDF[with(featImpDF, order(-absImportance)), ]

      # return selection
      selection <- featImpDF[1:numFeats,]
      selection$rank <- c(1:numFeats)
      selection$ids <- as.character(selection$ids)

      # deal with age/gender variables
      selection[selection$type=="age", colnames(selection)=="ids"] <- as.character(4265453)
      selection[selection$type=="gender", colnames(selection)=="ids"] <- as.character(2)

      # make sql query
      concept_data <- sapply(selection$ids, function(x) executeSQL(connection, schema, paste("SELECT A.* FROM @cdmSchema.concept A WHERE concept_id=",x,";", sep=""),dbms))

      # add concepts to output
      selection$concepts <- as.character(concept_data[rownames(concept_data)=="concept_name",])
      selection$code_source <- as.character(concept_data[rownames(concept_data)=="vocabulary_id",])

      return (selection)
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
      g <- ggplot(weightingsDF, aes(x=rank, y=importance))
      g <- g + geom_bar(stat='identity', color='firebrick', width=.5, fill='rosybrown4')
      g <- g + labs(x='', y="Feature Importance", title=paste("Feature Importance for",studyName))
      g <- g + scale_x_continuous(breaks=1:nrow(weightingsDF), labels=labels.wrap)
      g <- g + theme(text=element_text(size=12, color='black'), axis.text.y = element_text(size=10, angle=0, color='black'), axis.text.x=element_text(angle=0, size=16, color='black'))
      if (flag$model[1]=="LASSO") {
            g <- g + ylim(1.1*min(weightingsDF$importance),1.1*max(weightingsDF$importance))
      } else {
            g <- g + ylim(0,1.1*max(weightingsDF$importance))
      }
      #g <- g + theme(text = element_text(size=20),
                     #axis.text.x = element_text(angle=90, vjust=1))
      g <- g + coord_flip()
      g
      ggsave(plotSaveFile, width = 10, height = .4*nrow(weightingsDF), dpi = 400)

}



#' This function creates a summary metric for model training
#'
#' @description This function creates a new summary metric for model training, specific for unbalanced classes.  Inputs as specified in caret.
#'
#' @param data        A dataframe of the held-out example cases, with columns for 'obs', 'pred', 'T', 'F'.  'T' and 'F' have the probabilities of each of these classes
#' @param lev        Outcome factor levels for model
#' @param model       Character string of model used
#'
#' @details This function returns the F-score for model training optimization.  Beta is currently set at 2 - TODO: should make this edit-able in future version.
#'
#' @return f_score
#'
#' @examples \dontrun{
#'
#'  f_score <- f_score_calc(data, lev, model)
#'
#' }
#'
#' @export
f_score_calc <- function (data, lev=levels(data$obs), model=NULL) {

      out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))

      # get TP, FP, FN, FN
      TN <- nrow(data[(data$obs==data$pred) & (data$obs=='F'),])
      TP <- nrow(data[(data$obs==data$pred) & (data$obs=='T'),])
      FP <- nrow(data[(data$obs!=data$pred) & (data$pred=='T'),])
      FN <- nrow(data[(data$obs!=data$pred) & (data$pred=='F'),])
      #message(paste(TN, ', ', TP, ', ', FP, ', ', FN))
      beta <- 5
      f_score <- ((1+beta^2)*TP) / ( (1+beta^2)*TP + (beta^2)*FN + FP)
      out <- c(out, Fscore=f_score)
      return(out)
}



