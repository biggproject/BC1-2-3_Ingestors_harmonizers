library(biggr)
library("future")

benchmarking_path="reducer_lib/LongitudinalBenchmarking"

settings <- jsonlite::fromJSON(paste(benchmarking_path,"Settings.json", sep="/"))
settings <- append(settings,jsonlite::fromJSON(paste(benchmarking_path,"config.json", sep="/")))
source(paste(benchmarking_path, "LongitudinalBenchmarking.R", sep="/"))

save_data <-function(buildingId, buildingsRdf, timeseriesObject){
    rdf_file = "rdf.ttl"
    ts_file = "timeseries.json"
    rdflib::rdf_serialize(buildingsRdf,rdf_file,format="turtle")
    write(jsonlite::toJSON(timeseriesObject,dataframe="row"), file=ts_file)
    uri_1 = "http://master1.internal:9870/webhdfs/v1/"
    uri_2 = "?op=CREATE&user.name=ubuntu&overwrite=true"
    mpath = "user/ubuntu/tmp/longitudinalBenchmarking/errors"
    resp1 = httr::PUT(paste0(uri_1,paste(mpath,buildingId,rdf_file, sep="/"),uri_2))
    httr::PUT(resp1$url, body=setNames(list(file=httr::upload_file(rdf_file)),rdf_file))
    resp1 = httr::PUT(paste0(uri_1,paste(mpath,buildingId,ts_file, sep="/"),uri_2))
    httr::PUT(resp1$url, body=setNames(list(file=httr::upload_file(ts_file)),ts_file))
}

log_status <- function(key, status){
    executed_file = paste0(key,".status")
    uri_1 = "http://master1.internal:9870/webhdfs/v1/"
    uri_2 = "?op=CREATE&user.name=ubuntu&overwrite=true"
    mpath = "user/ubuntu/tmp/longitudinalBenchmarking/buildings_log/"
    resp1 = httr::PUT(paste0(uri_1,mpath,executed_file,uri_2))
    httr::PUT(resp1$url, body=status)
}

already_executed <- function(key){
    executed_file = paste0(key,".status")
    uri_1 = "http://master1.internal:9870/webhdfs/v1/"
    uri_2 = "?op=GETFILESTATUS"
    mpath = "user/ubuntu/tmp/longitudinalBenchmarking/buildings_log/"
    d = httr::GET(paste0(uri_1,mpath,executed_file,uri_2))
    if ("FileStatus" %in% names(httr::content(d))){
        return(TRUE)
    }else{
        return(FALSE)
    }
}


save_results<-function(results, buildingSubject){
    suppressMessages(suppressWarnings(library(rdflib)))
    suppressMessages(suppressWarnings(library(jsonlite)))
    suppressMessages(suppressWarnings(library(mongolite)))

    # Write the RDF file
    mongo_conn <- function(collection){
        mongolite::mongo(
            collection=collection,
            db=settings$MongoConnection$DB,
            url=sprintf(
                    "mongodb://%s:%s@%s:%s/%s",
                    settings$MongoConnection$User,settings$MongoConnection$Password,
                    settings$MongoConnection$Host,settings$MongoConnection$Port,settings$MongoConnection$DB
            )
        )
    }
    dir.create(settings$OutputDataDirectory, recursive = TRUE)
    # Write the RDF file
    write_rdf(
        object = results$results_rdf,
        file = paste(
            settings$OutputDataDirectory,
            sprintf(
                "%s.ttl",
                digest::digest(
                    object = results$key,
                    algo = "sha256",
                    serialize = T
                )
            ),
            sep="/"
        )
    )
#     #save rdf_results
#     rdf_file = sprintf(
#                 "%s.ttl",
#                 digest::digest(
#                     object = results$key,
#                     algo = "sha256",
#                     serialize = T
#                 )
#             )
#     biggr::write_rdf(results$results_rdf,rdf_file)
#     if(file.exists(rdf_file)){
#         write("file does not exists", stderr())
#     } else {
#         write("file does exist", stderr())
#     }
#     uri_1 = "http://master1.internal:9870/webhdfs/v1/"
#     uri_2 = "?op=CREATE&user.name=ubuntu&overwrite=true"
#     mpath = "user/ubuntu/tmp/longitudinalBenchmarking/results"
#     resp1 = httr::PUT(paste0(uri_1,paste(mpath,rdf_file, sep="/"),uri_2))
#     httr::PUT(resp1$url, body=setNames(list(file=httr::upload_file(rdf_file)),rdf_file))

    # Write the JSON files (time series)
    for(i in 1:length(results$results_ts)){
        write(
            jsonlite::toJSON(
                setNames(
                    list(results$results_ts[[i]]$basic),
                    names(results$results_ts)[i]),
                dataframe = "rows",na = "null"
            ),
            file=paste(
                settings$OutputDataDirectory,
                sprintf("%s.json", names(results$results_ts)[i]
                ),
                sep="/"
            )
        )
        columns_basic <- colnames(results$results_ts[[i]]$basic)
        # Upload the results for  cross-sectional benchmarking services, if they are available
        if("full" %in% names(results$results_ts[[i]])){
            columns_for_collection_name <- c("keyPerformanceIndicator","frequency")
            mongo_collection <- paste0(
                "SingleKPI-Building-",
                paste(
                    results$results_ts[[i]]$full[1,columns_for_collection_name],
                    collapse="-"
                )
            )
            results_aux <- results$results_ts[[i]]$full[,!(colnames(results$results_ts[[i]]$full) %in% columns_for_collection_name)]
            columns_unique_values <- colnames(results_aux)[!(colnames(results_aux) %in% c(
                columns_basic,"modelSubject"))]
            # ensure index for variables that can be filtered
#             if(mongo_conn(mongo_collection)$info()$stats$count==0L){
#                 mongo_conn(mongo_collection)$index(
#                     add=jsonlite::toJSON(
#                         as.list(
#                             setNames(
#                                 rep(1,length(columns_unique_values)),
#                                 nm=columns_unique_values
#                             )
#                         ),
#                         auto_unbox = T
#                     )
#                 )
#                 for(feat in columns_unique_values){
#                     mongo_conn(mongo_collection)$index(
#                         add=jsonlite::toJSON(as.list(setNames(1, nm=feat)),auto_unbox = T))
#                 }
#             }
            # update and upsert each timestep of results
            for(r in 1:nrow(results_aux)){
                mongo_conn(mongo_collection)$replace(
                    query=jsonlite::toJSON(jsonlite::unbox(as.data.frame(as.list(results_aux[r,columns_unique_values]))),
                                           POSIXt = "mongo",auto_unbox = T),
                    update=jsonlite::toJSON(jsonlite::unbox(as.data.frame(as.list(results_aux[r,]))),
                                            POSIXt = "mongo",auto_unbox = T),
                    upsert=T
                )
            }
        }
    }
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
            building_results <- longitudinal_benchmarking(buildingSubject, buildingsRdf, timeseriesObject,
                settings, libraryPath=benchmarking_path, updateHadoopStatus=T)
            for(results in building_results){
                results$key <- buildingSubject
                if(typeof(results)=="list"){
                    save_results(results, buildingSubject)
                    status <- paste(status, "success", sep=" ")
                } else{
                    write(results, stderr())
                }
            }
            if(buildingSubject %in% settings$storeFailed){
                save_data(buildingId, buildingsRdf, timeseriesObject)
            }
            sink()
        },
        error=function(e){
            write(sprintf("###### Error ###### %s",e),stderr())
        }
    )
    end_time <- Sys.time()
    diff_time = difftime(start_time, end_time)
    write(paste0("Building calculated in ",as.numeric(diff_time, units='secs')),  file=stderr())
}
