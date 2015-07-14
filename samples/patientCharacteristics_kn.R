# This is a script intended to take a look at the patient characteristics of our patient cases
#
# Written by KN, 13-Jul-2015



#####################################################################
#         Load up libraries and initiate db connection
#####################################################################
library(Aphrodite)
library(SqlRender)
library(plyr)
library(caret)
library(pROC)
library(DatabaseConnector)
library(ggplot2)
library(data.table)

folder = "/home/kniehaus/Aphrodite/" # Folder containing the R files and outputs, use forward slashes
setwd(folder)
source("R/settings_knTesting_FH.R")   #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it
source("R/functions.R")     # source this if changes have been made that aren't yet in the package

#Initiate connection to DB
connectionDetails <- createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmSchema, port=port)
conn <- connect(connectionDetails)


#####################################################################
#         Settings - change these to control various options
#####################################################################

descriptor <- 'cases_1stKeyword'

cases <- read.table(paste(saveFolder,studyName,'_cases.tsv',sep=''))
keywordList_FF <- read.table(paste(saveFolder,studyName,'_keywordlist_ed.tsv',sep=''), sep="\t", header=FALSE)
ignoreList_FF <- read.table(paste(saveFolder,studyName, '_ignorelist_ed.tsv',sep=''), sep="\t", header=FALSE)

pids <- cases
keywords <- keywordList_FF$V3   # V3 is the column we want
ignores <- ignoreList_FF$V3


#####################################################################
#         Get DOB
#####################################################################

#pt_dob <- executeSQL(conn, cdmSchema, paste("SELECT person_id, year_of_birth FROM @cdmSchema.person WHERE person_id IN ",as.character(pids[[1]]),";", sep=''), dbms)


#####################################################################
#         Get time at first mention of keywords
#####################################################################

datesAll <- list()
for (i in 1:(length(pids[[1]]))) {
    patients_list_df<- list()
    
    patients_list_df[[1]] <- executeSQL(conn, cdmSchema, paste("SELECT person_id, observation_date FROM @cdmSchema.observation WHERE observation_concept_id IN (",paste(keywords,collapse=","),",",paste(ignores,collapse=","),") AND qualifier_concept_id=0 AND person_id=",as.character(pids[[1]][i]),";",sep=''),dbms)
    
    patients_list_df[[2]] <- executeSQL(conn, cdmSchema, paste("SELECT person_id, condition_start_date AS observation_date FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (", paste(keywords,collapse=","), ",", paste(ignores,collapse=",") ,") AND person_id=", as.character(pids[[1]][i]),";",sep=''),dbms)        
    
    #Find the first date of the term mentions
    dates <- do.call(rbind, patients_list_df)
    dateFrom<-min(dates$observation_date)
    
    # Find DOB
    pt_dob <- executeSQL(conn, cdmSchema, paste("SELECT person_id, year_of_birth FROM @cdmSchema.person WHERE person_id=", as.character(pids[[1]][i]), ";", sep=''), dbms)

    # put into list
    days <- as.numeric(as.Date(dateFrom) - as.Date(as.character(pt_dob$year_of_birth), "%Y"))
    datesAll[[i]] <- days/365   # 365 days in year - approximate (don't even have exact dob, so is reasonable)
}

#####################################################################
#         Get time at first mention of keywords
#####################################################################

# plot histogram
plotSaveFile <- paste(saveFolder,studyName,'_ageAtDx_plot_', descriptor, '.jpg',sep='')
datesAll_df <- as.data.frame(unlist(datesAll))
colnames(datesAll_df)[1] <- "Years_old"
#hist(datesAll2)
qplot(Years_old, data=datesAll_df, geom="histogram", binwidth=3)
ggsave(plotSaveFile, width = 16, height = 9, dpi = 120)









