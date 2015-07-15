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
source("R/settings_knTesting_FH_14Jul2015.R")   #Load your settings.R  - usually found in ../R/settings.R   - Don't forget to edit it
source("R/functions.R")     # source this if changes have been made that aren't yet in the package
source("/home/kniehaus/Aphrodite/samples/patientCharacteristics_functions.R")
source("/home/kniehaus/knCode_FH/multiplot.R")

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
# get fv_all: list of features by type
load(file=paste(saveFolder,studyName,"_FULL_FV_CASES_",as.character(nCases),"_CONTROLS_",as.character(nControls),".Rda",sep=''))

pids <- cases
keywords <- keywordList_FF$V3   # V3 is the column we want
ignores <- ignoreList_FF$V3


#####################################################################
#         Get time at first mention of keywords
#####################################################################

datesOutput <- getDatesDx(conn, cdmSchema, keywords, ignores, pids)
datesAll_df <- datesOutput[[1]]
timeFollowed_df <- datesOutput[[2]]

# plot histogram of age at first term mention
plotSaveFile <- paste(saveFolder,studyName,'_ageAtDx_plot_', descriptor, '.jpg',sep='')
qplot(Years_old, data=datesAll_df, geom="histogram", binwidth=3)
ggsave(plotSaveFile, width = 16, height = 9, dpi = 120)

# plot histogram of time btw first and last term mention
plotSaveFile <- paste(saveFolder,studyName,'_timeFollowedCond_plot_', descriptor, '.jpg',sep='')
qplot(Time_followed, data=timeFollowed_df, geom="histogram", binwidth=1)
ggsave(plotSaveFile, width = 16, height = 9, dpi = 120)


#####################################################################
#         Get number of features of each type
#####################################################################

source("/home/kniehaus/Aphrodite/samples/patientCharacteristics_functions.R")

plotNumFeats(fv_all, pids, studyName, saveFolder)


for (i in 1:length(fv_all)) {
  # Get feature set
  fv_set <- fv_all[[i]]
  # Get relevant patients
  pids2 <- sapply(pids[[1]], as.character)
  fv_subset <- fv_set[fv_set$pid %in% pids2,]
  # Binarize features
  keepCols <- colnames(fv_subset)[!colnames(fv_subset) %in% "pid"]
  fv_binary <- fv_subset[,keepCols]
  fv_binary[fv_binary>0] <- 1
  # Get counts
  count_sumsB <- rowSums(fv_binary)   # binary
  #count_sumsB <- as.data.frame(count_sums)
  count_sumsT <- rowSums(fv_subset[,keepCols])  # total
  #count_sumsT <- as.data.frame(count_sumsT)
  # get feature type
  featType <- strsplit(colnames(fv_binary)[1], ":")[[1]][1]   # pids removed
  
  # plot
  #         plotSaveFile <- paste(saveFolder,studyName,'_numFeats_', featType, '_plot_', descriptor, '.jpg', sep='')
  #         message(plotSaveFile)
  #         a<- qplot(count_sumsB, data=count_sumsB, geom="histogram", binwidth=.05*max(count_sums))
  #         b <-qplot(count_sumsT, data=count_sumsT, geom="histogram", binwidth=.05*max(count_sums))
  #         
  #         multiplot(a, b, cols=2)
  #         ggsave(plotSaveFile, width = 16, height = 9, dpi = 120)
  
  par(mfrow=c(1,2))
  hist(count_sumsB)
  hist(count_sumsT)
  ggsave(plotSaveFile, width = 16, height = 9, dpi = 120)
  

