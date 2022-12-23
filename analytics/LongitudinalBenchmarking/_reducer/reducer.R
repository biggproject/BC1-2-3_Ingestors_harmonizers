library(magrittr)
library(rdflib)
library(reticulate)
library(data.table)

setwd("~/Developement/CIMNE/bigg_entrack/analytics/LongitudinalBenchmarking")
SCRIPT_PATH = "reducer"
source(paste0(SCRIPT_PATH, "/read_input.R"))

#source(paste0(SCRIPT_PATH, "/module.R"))
use_python("/usr/bin/python3", required = T)
source_python(paste0(SCRIPT_PATH, "/unpickle.py"))

reduce = function(key, values){
  #key<-x$key
  #values<-x$values
  list_data <- apply(data.frame(values), 1, read_pickle)
  timeseries <- list()
  ts_list <- list_data[lapply(list_data, function(x){x$meta})=="timeseries"]
  ts_df <- rbindlist(lapply(ts_list, function(x){c(x$point, x$hash)}), fill = TRUE)
  for (hash in unique(ts_df$V1)){
    device_ts = ts_df[ts_df$V1==hash]
    device_list <- list(subset(device_ts, select=c("start", "end", "value","isReal")))
    names(device_list)=hash
    timeseries = append(timeseries, device_list)
  }
  #rdf_serialize(graph,"output.ttl", format="turtle", namespace = "https://icaen.cat#")
  write("launch_module", stderr())
  print(key)
  print(timeseries)
  #launch_module(graph, timeseries)
}

stdin <- file('stdin', open='r')
stdin <- file('Rinput.txt', open='r')
loop(for (x in read_input(stdin)) {
  reduce(x$key, x$values)
})