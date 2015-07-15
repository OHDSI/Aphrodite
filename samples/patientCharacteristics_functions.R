# Helper functions for KN FindFH work
#
# Written 14-Jul-2015 by Kate Niehaus




#' This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @description This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @param conn         The database connection
#' @param cdmSchema     schema for database
#' @param keywords      List of keywords to include in search for condition
#' @param ignores       List of keywords to NOT include in search for condition
#' @param pids          List of patient id's of interest
#'
#' @details This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @return output: 
#' [1] datesAlldf: dataframe of all ages at first mention of a keyword term
#' [2] timeFollowed: dataframe of the time between the first and last term mention
#'
#' @examples \dontrun{
#'
#'  output <- getDatesDx(conn, cdmSchema, keywords, ignores, pids)
#'
#' }
#'
#' @export
getDatesDx <- function (conn, cdmSchema, keywords, ignores, pids) {

    datesAll <- list()
    timeFollowed <- list()
    for (i in 1:(length(pids[[1]]))) {
          patients_list_df<- list()
          
          message(pids[[1]][i])
          
          #TODO: change ignores
          patients_list_df[[1]] <- executeSQL(conn, cdmSchema, paste("SELECT person_id, observation_date FROM @cdmSchema.observation WHERE observation_concept_id IN (", paste(keywords,collapse=","), ",", paste(ignores,collapse=",") ,") AND qualifier_concept_id=0 AND person_id=",as.character(pids[[1]][i]),";",sep=''),dbms)
          
          patients_list_df[[2]] <- executeSQL(conn, cdmSchema, paste("SELECT person_id, condition_start_date AS observation_date FROM @cdmSchema.condition_occurrence WHERE condition_concept_id IN (", paste(keywords,collapse=","), ",", paste(ignores,collapse=",") ,") AND person_id=", as.character(pids[[1]][i]),";",sep=''),dbms)        
          
          #Find the first date of the term mentions
          dates <- do.call(rbind, patients_list_df)
          dateFrom<-min(dates$observation_date)
          dateMax <- max(dates$observation_date)
          
          # Find DOB
          pt_dob <- executeSQL(conn, cdmSchema, paste("SELECT person_id, year_of_birth FROM @cdmSchema.person WHERE person_id=", as.character(pids[[1]][i]), ";", sep=''), dbms)
          
          # put age at dx into list
          days <- as.numeric(as.Date(dateFrom) - as.Date(as.character(pt_dob$year_of_birth), "%Y"))
          datesAll[[i]] <- days/365   # 365 days in year - approximate (don't even have exact dob, so is reasonable)
          
          # put time followed into list
          daysFollowed <- as.numeric(as.Date(dateMax) - as.Date(dateFrom))
          timeFollowed[[i]] <- daysFollowed/365
    }
    
    datesAll_df <- as.data.frame(unlist(datesAll))
    colnames(datesAll_df)[1] <- "Years_old"
    
    timeFollowed_df <- as.data.frame(unlist(timeFollowed))
    colnames(timeFollowed_df)[1] <- "Time_followed"
    
    output <- list(datesAll_df, timeFollowed_df)
    
    return(output)
}





#' This function counts the number of distinct features for each patient and plots histograms
#'
#' @description This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @param flags         The R dataframe that contains all feature/model flags
#'
#' @details 
#'
#' @return 
#'
#' @examples \dontrun{
#'
#'  plotNumFeats(fv_all, pids, studyName, saveFolder)
#'
#' }
#'
#' @export
plotNumFeats <- function(fv_all, pids, studyName, saveFolder) {
      
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
        
      }
  
  
}







#' This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @description This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @param flags         The R dataframe that contains all feature/model flags
#'
#' @details 
#'
#' @return 
#'
#' @examples \dontrun{
#'
#'  timeFollowed <- getTimeFollowed(conn, cdmSchema, keywords, ignores, pids)
#'
#' }
#'
#' @export



#' #' This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @description This function connects to the DB to find the patient's age at the first time any of the keyword terms are mentioned in their notes
#'
#' @param flags         The R dataframe that contains all feature/model flags
#'
#' @details 
#'
#' @return 
#'
#' @examples \dontrun{
#'
#'  timeFollowed <- getTimeFollowed(conn, cdmSchema, keywords, ignores, pids)
#'
#' }
#'
#' @export