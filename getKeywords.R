######################################################################
# XPRESS - OHDSI Edition                                             #
# Beta release                                                       #
# Available at: http://www.github.com/jmbanda/OHDSI/Phenotyping/     #
# getKeywords.R                                                      #
# Requires: R and Java 1.6 or higher #                               #
######################################################################
# Install necessary packages if needed, remove comments
# install.packages("devtools")   
install_github("ohdsi/SqlRender")
install_github("ohdsi/DatabaseConnector")

library(devtools)
library(SqlRender)
library(DatabaseConnector)

###########################################################
# All parameters connection info and extras are in        #
# the file config.R, please make any appropiate changes   #
###########################################################
folder = "/home/jmbanda/Phenotyping/public_beta/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)

source("config.R")
###########################################################
# End of parameters. Make no changes after this #
###########################################################


connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)

conn <- connect(connectionDetails)
keyword_fetchingSql <- paste("SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",xpress_concept_name,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition';",sep = "")
renderedSql <- renderSql(keyword_fetchingSql, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
if (dbms=="postgresql") {
  concept_of_interest <- dbGetQueryPostgreSql(conn,translatedSql)  
} else {
  concept_of_interest <- dbGetQuery.ffdf(conn,translatedSql)  
}


if (nrow(concept_of_interest) == 0) {
  #No concept under that name found, maybe a typo look in the synonyms table
  keyword_fetchingSql <- paste("SELECT concept_id, concept_synonym_name FROM @cdmSchema.concept_synonym WHERE lower(concept_synonym_name)=lower('",xpress_concept_name,"');",sep = "")
  renderedSql <- renderSql(keyword_fetchingSql, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
  translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
  if (dbms=="postgresql") {
      concept_of_interest <- dbGetQueryPostgreSql(conn,translatedSql)
  } else {
    concept_of_interest <- dbGetQuery.ffdf(conn,translatedSql)  
  }      
  if (nrow(concept_of_interest) == 0) {
    stop("No concepts found with the string provided, please try another one.")
  }
}


keywordlist_clean_df<- list()
ignorelist_df<- list()

for (loopC in 1:nrow(concept_of_interest)) {
  currentConcept_id <- concept_of_interest[loopC,1]
  currentConcept_name <- concept_of_interest[loopC,2]
  getRelatedTermsSql <- paste("SELECT ALT.concept_id, ALT.concept_name, ALT.related_concept_id, ALT.related_concept_name FROM ( (SELECT A.concept_id, A.concept_name, B.descendant_concept_id as related_concept_id, C.concept_name as related_concept_name  FROM (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",currentConcept_name,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition') as A, @cdmSchema.concept_ancestor as B, (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE invalid_reason IS NULL AND domain_id = 'Condition') as C WHERE A.concept_id = B.ancestor_concept_id AND C.concept_id = B.descendant_concept_id) UNION (SELECT A.concept_id, A.concept_name, B.ancestor_concept_id as related_concept_id, C.concept_name as related_concept_name  FROM (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE lower(concept_name) =lower('",currentConcept_name,"') AND standard_concept = 'S' AND invalid_reason IS NULL AND domain_id = 'Condition') as A, @cdmSchema.concept_ancestor as B, (SELECT concept_id, concept_name FROM @cdmSchema.concept WHERE invalid_reason IS NULL AND domain_id = 'Condition') as C WHERE A.concept_id = B.descendant_concept_id AND C.concept_id = B.ancestor_concept_id) UNION(SELECT A.concept_id_1 as concept_id, C.concept_name, A.concept_id_2 as related_concept_id, B.concept_name as related_concept_name FROM @cdmSchema.concept_relationship as A, @cdmSchema.concept as B, @cdmSchema.concept as C WHERE concept_id_1 = ",currentConcept_id," AND A.invalid_reason IS NULL AND A.concept_id_2 = B.concept_id AND A.concept_id_1=C.concept_id AND B.domain_id = 'Condition' AND C.domain_id='Condition')) as ALT ORDER BY ALT.related_concept_id;",sep='')
  renderedSql <- renderSql(getRelatedTermsSql, cdmSchema=cdmSchema, resultsSchema=resultsSchema, studyName = studyName, sourceName=sourceName)$sql
  translatedSql <- translateSql(renderedSql, sourceDialect = "postgresql", targetDialect = dbms)$sql
  if (dbms=="postgresql") {  
    keywordlist_df <- dbGetQueryPostgreSql(conn,translatedSql)
  } else {
    keywordlist_df <- dbGetQuery.ffdf(conn,translatedSql)  
  }  
  #Clean the keyword list of the keyword selected
  keywordlist_clean_df[[loopC]]<-subset(keywordlist_df, tolower(c(related_concept_name)) != tolower(c(xpress_concept_name)))
  #Move the removed terms to the actual ignore file
  ignorelist_df[[loopC]]<-subset(keywordlist_df, tolower(c(related_concept_name)) == tolower(c(xpress_concept_name)))
}

#Compile all ignore lists and keywords lists into one full file each
ignorelist_ALL <- do.call(rbind, ignorelist_df)
keywordlist_ALL <- do.call(rbind,keywordlist_clean_df)
keywordlist_ALL <- keywordlist_ALL[-nrow(keywordlist_ALL),] 
write.table(keywordlist_ALL, file=paste('keywordlist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
write.table(ignorelist_ALL, file=paste('ignorelist.tsv',sep=''), quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)
#Clean some dataframes
remove('keywordlist_clean_df','keywordlist_df','ignorelist_df', 'keywordlist_ALL', 'ignorelist_ALL')
message(paste("Keywords.tsv and ignore.tsv have been successfully created for ",xpress_concept_name,sep = ""))
dbDisconnect(conn)