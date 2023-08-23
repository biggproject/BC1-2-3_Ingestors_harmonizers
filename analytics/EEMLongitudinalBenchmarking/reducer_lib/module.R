library(biggr)
benchmarking_path="reducer_lib/EnergyEfficiencyMeasuresAssessment"

settings <- jsonlite::fromJSON(paste(benchmarking_path,"Settings.json", sep="/"))
settings <- append(settings,jsonlite::fromJSON(paste(benchmarking_path,"config.json", sep="/")))
source(paste(benchmarking_path, "EEMsAssessment.R", sep="/"))
source(paste(benchmarking_path, "Utils.R", sep="/"))

save_data <-function(buildingId, buildingsRdf, timeseriesObject){
    rdf_file = "rdf.ttl"
    ts_file = "timeseries.json"
    rdflib::rdf_serialize(buildingsRdf,rdf_file,format="turtle")
    write(jsonlite::toJSON(timeseriesObject,dataframe="row"), file=ts_file)
    uri_1 = "http://master1.internal:9870/webhdfs/v1/"
    uri_2 = "?op=CREATE&user.name=ubuntu&overwrite=true"
    mpath = "user/ubuntu/tmp/EEMLongitudinalBenchmarking/errors"
    resp1 = httr::PUT(paste0(uri_1,paste(mpath,buildingId,rdf_file, sep="/"),uri_2))
    httr::PUT(resp1$url, body=setNames(list(file=httr::upload_file(rdf_file)),rdf_file))
    resp1 = httr::PUT(paste0(uri_1,paste(mpath,buildingId,ts_file, sep="/"),uri_2))
    httr::PUT(resp1$url, body=setNames(list(file=httr::upload_file(ts_file)),ts_file))
}

log_status <- function(key, status){
    executed_file = paste0(key,".status")
    uri_1 = "http://master1.internal:9870/webhdfs/v1/"
    uri_2 = "?op=CREATE&user.name=ubuntu&overwrite=true"
    mpath = "user/ubuntu/tmp/EEMLongitudinalBenchmarking/buildings_log/"
    resp1 = httr::PUT(paste0(uri_1,mpath,executed_file,uri_2))
    httr::PUT(resp1$url, body=status)
}

already_executed <- function(key){
    executed_file = paste0(key,".status")
    uri_1 = "http://master1.internal:9870/webhdfs/v1/"
    uri_2 = "?op=GETFILESTATUS"
    mpath = "user/ubuntu/tmp/EEMLongitudinalBenchmarking/buildings_log/"
    d = httr::GET(paste0(uri_1,mpath,executed_file,uri_2))
    if ("FileStatus" %in% names(httr::content(d))){
        return(TRUE)
    }else{
        return(FALSE)
    }
}

save_and_load_results<-function(results, settings, buildingSubject){
    # write results to files
    save_results(results, settings)
    # load results to neo4j
    user <- settings$userMap[strsplit(buildingSubject, "#", fixed=T)[[1]][1]][[1]]
    pyos <- import("os")
    load_ttl <- import("load_ttl")
    load_ttl$load_ttl_to_neo4j(settings$OutputDataDirectory, user)
    unlink(settings$OutputDataDirectory, recursive = TRUE)
}

launch_module <- function(buildingSubject, buildingsRdf, timeseriesObject){
    options(bitmapType='cairo')
    start_time <- Sys.time()
    status <- ""
    buildingId <- biggr::get_building_identifiers(buildingSubject)
    if (already_executed(buildingId)){
        return()
    }
    log_status(buildingId, "starting")
    save_failed <- FALSE
    tryCatch(
        {
            building_results <- EEMs_assessment(buildingSubject, buildingsRdf, timeseriesObject,
                settings, libraryPath=benchmarking_path, updateHadoopStatus=T)
            for(results in building_results){
                if(typeof(results)=="list"){
                    results$key <- buildingSubject
                    save_and_load_results(results, settings, buildingSubject)
                    status <- paste(status, "success", sep=" ")
                } else{
                    write(results, stderr())
                }
            }
            if(buildingSubject %in% settings$storeFailed){
                save_data(buildingId, buildingsRdf, timeseriesObject)
            }
        },
        error=function(e){
            write(sprintf("###### Error ###### %s",e),stderr())
        }
    )
    end_time <- Sys.time()
    diff_time = difftime(start_time, end_time)
    write(paste0("Building calculated in ",as.numeric(diff_time, units='secs')),  file=stderr())
}
