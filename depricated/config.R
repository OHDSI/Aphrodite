######################################################################################
######################################################################################
###  Configuration file - do not modiy when building an end-to-end phenotype       ###
###                                                                                ###
######################################################################################
######################################################################################

### Connection Variables ###

cdmSchema = "ohdsiv5"
resultsSchema = "results_schema"
sourceName = "source_name"
dbms = "postgresql" #Should be "sql server", "oracle", "postgresql" or "redshift"

user <- "achilles" #Change when in production to: NULL
pw <- "achilles" #Change when in production to: NULL
server <- "localhost/jmbanda"  #Change when in production to: "server_name"
port <- "5432" #Change when in production to: NULL


### XPRESS VARIABLES ###
studyName <-'MI'   #String for saving objects prefix
outcomeName <- 'MI' #String for model objects prefix
nCases = 100 # Number of patients to use as cases
nControls = 100 #Number of patients to use as controls

### Key variable to change - used in getKeywords.R
#xpress_concept_name <- 'Type 2 diabetes mellitus'
xpress_concept_name <- 'myocardial infarction'

#### Flags will determine if we use that feature set or not
flag_drugexposures=1
flag_observations=1
flag_visits=1
flag_labs=0
saveALLresults=1
#Type of model to build:  Only LASSO and RF have been implemented
flag_model <-'LASSO' # LASSO or 'RF'
features_mode <- 'frequency'   #Can be Boolean or frequency - boolean only requires presence and frequency keeps the counts
plots=1
## Predictions file
toPredict <- "cases_MI.txt"


