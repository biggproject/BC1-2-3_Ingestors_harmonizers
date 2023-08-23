mongo_conn <- function(collection, settings){
  mongolite::mongo(
    collection=collection,
    db=settings$MongoConnection$DB,
    url=sprintf(
      "mongodb://%s:%s@%s:%s/%s",
      settings$MongoConnection$User,
      settings$MongoConnection$Password,
      settings$MongoConnection$Host,
      settings$MongoConnection$Port,
      settings$MongoConnection$DB
    )
  )
}

mongo_check <- function(collection, settings){
  return(!is.null(
    tryCatch(
      mongo_conn(collection,settings),
      error=function(e) NULL
    )))
}

save_results <- function(results, settings, ensure_index=F){
  
  suppressMessages(suppressWarnings(library(rdflib)))
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(mongolite)))
  
  # Write the RDF file
  write_rdf(object = results$results_rdf,
            file = paste(settings$OutputDataDirectory,
                         sprintf("%s.ttl", digest::digest(
                           results$key,
                           algo = "sha256", serialize = T)), sep="/"))
  
  # Write the JSON files (time series)
  for(i in 1:length(results$results_ts)){
    write(jsonlite::toJSON(
      setNames(list(results$results_ts[[i]]$basic),names(results$results_ts)[i]),
      dataframe = "rows",na = "null"),
      file=paste(settings$OutputDataDirectory,
                 sprintf("%s.json",names(results$results_ts)[i]),
                 sep="/") )
    columns_basic <- colnames(results$results_ts[[i]]$basic)
    
    # Upload the results for EEM assessment services, if they are available
    columns_for_collection_name <- c("keyPerformanceIndicator","frequency")
    mongo_collection <- paste0(
      "SingleKPI-",
      results$results_ts[[i]]$full$individualType[1],"-",
      paste(results$results_ts[[i]]$full[1,columns_for_collection_name],collapse="-"))
    if(mongo_check(mongo_collection, settings) &&
       ("full" %in% names(results$results_ts[[i]])) &&
       nrow(results$results_ts[[i]]$full)>0){
      results_aux <- results$results_ts[[i]]$full[,!(colnames(results$results_ts[[i]]$full) %in% columns_for_collection_name)]
      columns_unique_values <- colnames(results_aux)[!(colnames(results_aux) %in% c(
        columns_basic,"modelSubject"))]
      # ensure index for variables that can be filtered
      if(ensure_index){
          if(mongo_conn(mongo_collection, settings)$info()$stats$count==0L){
            mongo_conn(mongo_collection, settings)$index(
              add=jsonlite::toJSON(as.list(setNames(rep(1,length(columns_unique_values)),
                                                    nm=columns_unique_values)),auto_unbox = T))
            for(feat in columns_unique_values){
              mongo_conn(mongo_collection, settings)$index(
                add=jsonlite::toJSON(as.list(setNames(1, nm=feat)),auto_unbox = T))
            }
          }
      }
      # update and upsert each timestep of results
      for(r in 1:nrow(results_aux)){
        mongo_conn(mongo_collection, settings)$replace(
          query=jsonlite::toJSON(jsonlite::unbox(as.data.frame(as.list(results_aux[r,columns_unique_values]))),auto_unbox = T,
                                 POSIXt = "mongo"),
          update=jsonlite::toJSON(jsonlite::unbox(as.data.frame(as.list(results_aux[r,]))),auto_unbox = T,
                                  POSIXt = "mongo"),upsert=T)
      }
    }
  }
}

run_EEMs_assessment <- function(buildingsRdf, timeseriesObject, settings){
  library(biggr)
  allBuildingSubjects <- get_all_buildings_list(buildingsRdf)
  for (buildingSubject in allBuildingSubjects){
    # buildingSubject <- allBuildingSubjects[4] 
    tryCatch(
      {
        building_results <- EEMs_assessment(buildingSubject, buildingsRdf, timeseriesObject, settings,
                                            libraryPath = ".", updateHadoopStatus = F)
        building_results$key <- buildingSubject
        save_results(building_results, settings)
      }, 
      error=function(e){NULL}
    )
  }
}
