# @file functions
#
# Copyright 2015-2018 Observational Health Data Sciences and Informatics
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
# @author Georgia State University - Panacea Lab
# @author Juan M. Banda

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$t2_concept <- renderText({ input$txtaphrodite_concept_name })
    output$t2_i_concept <- renderText({ input$txtaphrodite_concept_name })

    output$ohdsi_server1 <- renderText({ input$txtserver })
    output$ohdsi_server2 <- renderText({ input$txtserver })

    output$table <- DT::renderDataTable(DT::datatable({
        setwd(input$txtfolderPath)
        dataKeywords<-read.table(paste(input$txtstudyName,'keywordlistAF.tsv',sep=''), sep="\t", header=FALSE)
        colnames(dataKeywords) <- c("Label concept_id","concept_name","Related concept_id","concept_name")
        data <- dataKeywords
        data
    }))

    output$tableI <- DT::renderDataTable(DT::datatable({
        setwd(input$txtfolderPath)
        dataKeywordsI<-read.table(paste(input$txtstudyName,'ignorelistAF.tsv',sep=''), sep="\t", header=FALSE)
        colnames(dataKeywordsI) <- c("Label concept_id","concept_name","Related concept_id","concept_name")
        dataI <- dataKeywordsI
        dataI
    }))

   output$tableCohort <- DT::renderDataTable(DT::datatable({
        setwd(input$txtfolderPath)
        datacohorts<-read.table(paste('cohorts_table.csv',sep=''), sep=",", header=FALSE)
        colnames(datacohorts) <- c("Cohort Id", "Cohort Name")
        datacoh <- datacohorts
        datacoh
    }))

    output$tableConcept <- DT::renderDataTable(DT::datatable({
        setwd(input$txtfolderPath)
        dataconceptSet<-read.table(paste('concept_set.csv',sep=''), sep=",", header=FALSE)
        colnames(dataconceptSet) <- c("Concept Set Id", "Concept Set Name")
        dataconcepts <- dataconceptSet
        dataconcepts
    }))

})
