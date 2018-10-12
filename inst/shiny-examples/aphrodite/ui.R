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
# Define UI for application that draws a histogram
shinyUI(fluidPage(
            img(src='aphrodite_logo.jpg', align = "center"),
            titlePanel(""),

            tabsetPanel(
                tabPanel("General Settings",
                         h1("Aphrodite Concept/Term label:"),
                         textInput("txtaphrodite_concept_name", "", value ="myocardial infarction"),

                    fluidRow(
                     column(4,
                         h1("Database Settings"),
                         hr(),
                         textInput("txtcdmSchema", "cdmSchema:"),
                         textInput("txtresultsSchema", "resultsSchema:"),
                         textInput("txtsourceName", "sourceName:"),
                         selectInput("dbms",
                                     "DBMS:",
                                     c("postgresql","oracle","redshift","PDW","sqlserver"), selected = "postgresql"),
                         textInput("txtuser", "User Name:"),
                         passwordInput("txtpw", "User PWD:"),
                         textInput("txtserver", "Server Name:", value ="localhost/ohdsi_prod"),
                         textInput("txtport", "Port:", value ="5432"),
                         hr()
                     ),
                     column(4,
                         h1("Model Settings"),
                         hr(),
                         textInput("txtfolderPath", "Output Folder Path:", value ="/home/jmbanda/OHDSI/Aphrodite-TEMP/"),
                         textInput("txtstudyName", "Study Name (No Spaces):", value ="DEMO-NoteNLP-full"),
                         textInput("txtoutcomeName", "Outcome Name (No Spaces):", value ="DEMO-NoteNLP-full"),
                         textInput("txtmodelType", "ML Algorithm (LASSO, RF, SVM, etc.): ", value="LASSO"),
                         textInput("txtnCases", "Number of Cases:", value ="50"),
                         textInput("txtnControls", "Number of Controls:", value ="100")
                     ),
                     column(4,
                         h1("Aphrodite Settings"),
                         hr(),
                         checkboxGroupInput("chkFeatures", label = "Feature Spaces to Use",
                                            choices = list("Conditions" = 1, "Observations" = 2, "Procedures" = 3, "Measurements" = 4, "NoteNLP" = 5, "DrugExposures" = 6),
                                            selected = list(1,2,3,4,5,6)),
                         h2(),
                         checkboxGroupInput("chkDomains", label = "Search Domains for Keywords",
                                            choices = list("Conditions" = 1, "Observations" = 2, "Procedures" = 3, "Measurements" = 4, "NoteNLP" = 5, "DrugExposures" = 6),
                                            selected = list(1,2))
                     )
                    ),
                         hr(),
                         fluidRow(column(3, verbatimTextOutput("genCode")))
                ),
                tabPanel("Concept/Term Selection",
                         tabsetPanel(
                             tabPanel("Include keyword Selection",
                                     h1("Aphrodite Concept/Term label Selected:"),
                                     h2(textOutput("t2_concept")),
                                     hr(),
                                     DT::dataTableOutput("table"),
                                     actionButton("deleteRows", "Delete Rows"),
                                     hr()
                             ),
                             tabPanel("Ignore keyword Selection",
                                      h1("Aphrodite Concept/Term label Selected:"),
                                      h2(textOutput("t2_i_concept")),
                                      hr(),
                                      DT::dataTableOutput("tableI"),
                                      actionButton("deleteRowsI", "Delete Rows"),
                                      hr()
                             )
                         )

                ),
                tabPanel("Use existing Atlas Cohort",
                         h1("Cohorts Found on the Server:"),
                         h2(textOutput("ohdsi_server1")),
                         hr(),
                         DT::dataTableOutput("tableCohort"),
                         actionButton("useCohort", "Select Cohort"),
                         hr()
                ),
                tabPanel("Use existing Atlas Concept Set",
                         h1("Concept Sets Found on the Server:"),
                         h2(textOutput("ohdsi_server2")),
                         hr(),
                         DT::dataTableOutput("tableConcept"),
                         actionButton("useConceptSet", "Select Concept Set"),
                         hr()
                )
            )
        )
)
