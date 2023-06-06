library(magrittr)
library(rdflib)
library(reticulate)
library(data.table)
library(jsonlite)
setwd(getwd())
current_path <- "reducer_lib"
benchmarking_path="reducer_lib/LongitudinalBenchmarking"
settings <- fromJSON(paste(benchmarking_path, "config.json", sep="/"))
source(paste(current_path, "read_input.R", sep="/"))
source(paste(current_path, "module.R", sep="/"))

use_python(settings$PYTHON3_BIN, required = T)

source_python(paste(current_path,"unpickle.py", sep="/"))
source_python(paste(current_path,"extract_building.py", sep="/"))

get_building_rdf<-function(building_subject){
  for (f in list.files(pattern=".*.ttl")){
    invisible(buildingsRdf <- rdf_parse(get_subgraph_from_rdf(f, building_subject) , format="turtle"))
    if (length(buildingsRdf) > 0){
      return(buildingsRdf)
    }
  }
  return(NULL)
}

reduce = function(index, values){
  #key<-x$key$X1
  #values<-x$values
  list_data <- apply(data.frame(values), 1, read_pickle)
  key <- list_data[[1]]$building
  timeseries <- list()
#   ts_list <- list_data[lapply(list_data, function(x){x$meta})=="timeseries"]
#   ts_df <- rbindlist(lapply(ts_list, function(x){c(x$point, x$hash)}), fill = TRUE)
#   for (hash in unique(ts_df$V1)){
#     device_ts = ts_df[ts_df$V1==hash]
#     device_list <- list(subset(device_ts, select=c("start", "end", "value","isReal")))
#     names(device_list)=hash
#     timeseries = append(timeseries, device_list)
#   }
  ts_shared_list <- list_data[lapply(list_data, function(x){x$meta})=="shared_file"]
  write(paste0(key, " key"), file=stderr())
  uri_1 = "http://master1.internal:9870/webhdfs/v1/"
  uri_2 = "?op=OPEN"
  for (file in ts_shared_list){
    resp = httr::GET(paste0(uri_1,file$file,uri_2))
    resp = httr::content(resp, as="text", content="UTF-8")
    table = read.table(text=resp, sep=";", header=FALSE, col.names=c("start", "end", "value", "isReal"), encoding = "UTF-8")
    table["isReal"] = ifelse("True", TRUE, FALSE)
    table["value"] = as.numeric(table$value)
    device_list = list(table)
    names(device_list)=file$hash
    timeseries = append(timeseries, device_list)
  }
  graph <- get_building_rdf(key)
  if(is.null(graph)){
    write(paste0("\t", key, " not found"), file=stderr())
  }else{
    write(paste0("\t", key, " launch_module"), file=stderr())
    launch_module(key, graph, lapply(timeseries, FUN=function(x){return(as.data.frame(x))}))
  }
}

stdin <- file('stdin', open='r')
# stdin <- file('to_remove.txt', open='r')
loop(for (x in read_input(stdin)) {
  write("reporter:counter: CUSTOM, buildings,1", file=stderr())
  reduce(x$key$X1, x$values)
})
