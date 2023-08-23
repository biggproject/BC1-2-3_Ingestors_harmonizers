source("Utils.R")
source("EEMsAssessment.R")

# Load configuration paths and settings for the algorithm
settings <- jsonlite::fromJSON("Settings.json")
settings <- append(settings,jsonlite::fromJSON("config.json"))

# Unzip input data if is needed
if(!dir.exists(settings$InputDataDirectory)){
  unzip(paste0(settings$InputDataDirectory,".zip"),exdir = settings$InputDataDirectory,junkpaths = T)
}

# Read buildings TTL file and time series
buildingsRdf <- suppressMessages(
  rdflib::rdf_parse(
    list.files(settings$InputDataDirectory,".ttl",full.names = T)[1], 
    format = "turtle")
)
timeseriesObject <- settings$InputDataDirectory

# Run the Energy Efficiency Measures assessment
run_EEMs_assessment(buildingsRdf, timeseriesObject, settings)
