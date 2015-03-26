######################################################################
# XPRESS - OHDSI Edition                                             #
# Beta release                                                       #
# Available at: http://www.github.com/jmbanda/OHDSI/Phenotyping/     #
# buildModel.R                                                       #
# Requires: R and Java 1.6 or higher #                               #
######################################################################
# Install necessary packages if needed, remove comments
#install.packages("devtools")   
#install_github("ohdsi/SqlRender")
#install_github("ohdsi/DatabaseConnector")
#install.packages("caret")
#install.packages("pROC")

library(caret)
library(pROC)

source('config.R')

#Load the cases and control pids - mostly for labeling
cases_pids <- read.table('cases.tsv', sep="\t", header=FALSE)
controls_pids <- read.table('controls.tsv', sep="\t", header=FALSE)

feature_vectors <- list()

featuresets=1
if (flag_drugexposures) {
  load(paste(studyName,"-FULL_FV_drugexposures_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
  feature_vectors[[featuresets]]<-FV_de
  featuresets = featuresets+1
}
if (flag_visits) {
  load(paste(studyName,"-FULL_FV_visits_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
  feature_vectors[[featuresets]]<-FV_v
  featuresets = featuresets+1
}
if (flag_observations) {
  load(paste(studyName,"-FULL_FV_observations_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
  feature_vectors[[featuresets]]<-FV_ob
  featuresets = featuresets+1
}
if (flag_labs) {
  load(paste(studyName,"-FULL_FV_labs_Cases",as.character(nCases),"_Controls",as.character(nControls),".Rda",sep=''))
  feature_vectors[[featuresets]]<-FV_lab
  featuresets = featuresets+1
}

#Merge all dataframes/Feature vectors for the different sources and have a big list of them
pp_total = Reduce(function(...) merge(..., by="pid", all=T), feature_vectors)

#Get selection of FV we wnat
ppv_set<-pp_total[1:nrow(pp_total),2:ncol(pp_total)]
#Convert features to boolean or leave as frequency
if (tolower(c(features_mode)) == 'boolean') {
  ppv_set[ppv_set > 0] <- 1  #
}

labels <- pp_total$pid %in% cases_pids$V
mode(labels) <- "integer"

replace(labels, labels==0, 'F')
replace(labels, labels==1, 'T')

ppv_set[,paste(outcomeName)] <- labels

predictorsNames <- names(ppv_set)[names(ppv_set) != outcomeName]

if (flag_model=='LASSO') {
    ################################################
    # glmnet LASSO                                ##
    ################################################
    # split data into training and testing chunks
    set.seed(567)
    splitIndex <- createDataPartition(ppv_set[,outcomeName], p = .75, list = FALSE, times = 1)
    trainDF <- ppv_set[ splitIndex,]
    testDF  <- ppv_set[-splitIndex,]
    # create caret trainControl object to control the number of cross-validations performed
    objControl <- trainControl(method='cv', number=5, returnResamp='none')
    # run model
    objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName], method='glmnet',  metric = "RMSE", trControl=objControl)
    # get predictions on your testing data
    predictions <- predict(object=objModel, testDF[,predictorsNames])
    auc <- roc(testDF[,outcomeName], predictions)
    ###### Model Ouputs to file #############
    sink(paste('LASSO output for-',outcomeName,'-Cases-',as.character(nCases),'-Controls-',as.character(nControls),'.txt',sep=''))
    cat(paste('Results for LASSO Model for-',outcomeName,' using ',as.character(nCases),' Cases and ',as.character(nControls),' Controls. \n\n',sep=''))
    print(auc$auc)
    cat("\n")
    cat(postResample(pred=predictions, obs=testDF[,outcomeName]))
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
    ###### Save Model to file #############
    save(objModel,file=paste(flag_model, " MODEL FILE FOR ",outcomeName,".Rda",sep=''))
    #Save Predictors for model
    save(predictorsNames,file=paste(flag_model, " PREDICTORS FOR ",outcomeName,".Rda",sep=''))
    ###### Optional Plots   ###############
    if (plots) {
        png(file = paste("Lasso Variables Importance -",outcomeName,".png",sep=''), width = 1200, height = 1000, res=120)
        vimp <- varImp(objModel, scale=F, top=20)
        results <- data.frame(row.names(vimp$importance),vimp$importance$Overall)
        results$VariableName <- rownames(vimp)
        colnames(results) <- c('VariableName','Weight')
        results <- results[order(results$Weight),]
        results <- results[(results$Weight != 0),]
        
        par(mar=c(5,15,4,2)) # increase y-axis margin. 
        xx <- barplot(results$Weight, width = 0.85, 
                      main = paste("Lasso Variable Importance -",outcomeName), horiz = T, 
                      xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE, 
                      col = ifelse((results$Weight > 0), 'blue', 'red')) 
        axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)
        dev.off()
        plot.new()
        png(file = paste("Lasso ROC Curve -",outcomeName,".png",sep=''), width = 1200, height = 1000, res=120)
        plot.roc(testDF[,outcomeName], predictions, smooth=TRUE, percent=TRUE, main = paste("Lasso ROC Curve -",outcomeName), horiz = T,)        
        dev.off()        
    }
}

if (flag_model=='RF') {
###########################################
### Random Forest #########################
###########################################
    set.seed(765)
    splitIndex <- createDataPartition(ppv_set[,outcomeName], p = .75, list = FALSE, times = 1)
    trainDF <- ppv_set[ splitIndex,]
    testDF  <- ppv_set[-splitIndex,]
    
    # create caret trainControl object to control the number of cross-validations performed
    objControl <- trainControl(method='cv', number=5, returnResamp='none')
    objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName], method='rf',  metric = "RMSE", trControl=objControl, importance = TRUE)
    
    # get predictions on your testing data
    predictions <- predict(object=objModel, testDF[,predictorsNames])
    auc <- roc(testDF[,outcomeName], predictions)

    ###### Model Ouputs to file #############
    sink(paste('RF output for-',outcomeName,'-Cases-',as.character(nCases),'-Controls-',as.character(nControls),'.txt',sep=''))
    cat(paste('Results for RF Model for-',outcomeName,' using ',as.character(nCases),' Cases and ',as.character(nControls),' Controls. \n\n',sep=''))
    print(auc$auc)
    cat("\n")
    confusionMatrix(predictions, testDF[,outcomeName])
    cat("\n")
    cat(postResample(pred=predictions, obs=testDF[,outcomeName]))
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
    ###### Save Model to file #############
    save(objModel,file=paste(flag_model, " MODEL FILE FOR ",outcomeName,".Rda",sep=''))
    #Save Predictors for model
    save(predictorsNames,file=paste(flag_model, " PREDICTORS FOR ",outcomeName,".Rda",sep=''))
    ###### Optional Plots   ###############
    if (plots) {
      png(file = paste("RF Variables Importance -",outcomeName,".png",sep=''), width = 1200, height = 1000, res=120)
      # display variable importance on a +/- scale 
      vimp <- varImp(objModel, scale=F, top=20)
      results <- data.frame(row.names(vimp$importance),vimp$importance$Overall)
      results$VariableName <- rownames(vimp)
      colnames(results) <- c('VariableName','Weight')
      results <- results[order(results$Weight),]
      results <- results[(results$Weight != 0),]
      
      par(mar=c(5,15,4,2)) # increase y-axis margin. 
      xx <- barplot(results$Weight, width = 0.85, 
                    main = paste("Random Forest Variable Importance -",outcomeName), horiz = T, 
                    xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE, 
                    col = ifelse((results$Weight > 0), 'blue', 'red')) 
      axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)
      dev.off()
      plot.new()
      png(file = paste("RF ROC Curve -",outcomeName,".png",sep=''), width = 1200, height = 1000, res=120)
      plot.roc(testDF[,outcomeName], predictions, percent=TRUE, main = paste("RF ROC Curve -",outcomeName), horiz = T,)        
      dev.off()              
    }
}
