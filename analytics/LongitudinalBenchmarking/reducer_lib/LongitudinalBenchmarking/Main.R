source("Utils.R")
source("LongitudinalBenchmarking.R")

# Load configuration paths and settings for the algorithm
settings <- jsonlite::fromJSON("Settings.json")
settings <- append(settings,jsonlite::fromJSON("config.json"))
settings$InputDataDirectory<-"/Users/eloigabal/Developement/CIMNE/bigg_entrack/extract_data"
settings$PYTHON_BIN<-"venv/bin/python3.10"
settings$MLFlow$MLFLOW_BIN<-"venv/bin/mlflow"
settings$MLFlow$TrackingUri<-"http://localhost:5000"

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

# Run the benchmarking
run_benchmarking(buildingsRdf, timeseriesObject, settings)
