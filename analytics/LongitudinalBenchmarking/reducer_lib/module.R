library(qgam)
imports = c("biggr", "data.table","jsonlite","ggplot2", "lubridate",
"gridExtra", "plotly", "padr", "htmlwidgets", "carrier","mlflow","fs",
"tidyr","digest","pryr")
for(pack in imports){suppressMessages(suppressWarnings(library(pack, character.only = TRUE)))}

source("reducer_lib/HourlyDynamicModel.R")
source("reducer_lib/HourlyBaselineModel.R")
source("reducer_lib/MonthlyDynamicModel.R")
source("reducer_lib/MonthlyBaselineModel.R")
source("reducer_lib/YearlyDynamicModel.R")

settings <- fromJSON("reducer_lib/Settings.json")
settings <- append(settings,fromJSON("reducer_lib/Rconfig.json"))

save_data <-function(buildingId, buildingsRdf, timeseriesObject){
    rdf_file = paste0(buildingId,"/rdf.ttl")
    ts_file = paste0(buildingId,"/timeseries.json")
    rdflib::rdf_serialize(buildingsRdf,rdf_file,format="turtle")
    write(jsonlite::toJSON(timeseriesObject,dataframe="row"), file=ts_file)
    uri_1 = "http://master1.internal:9870/webhdfs/v1/"
    uri_2 = "?op=CREATE&user.name=ubuntu&overwrite=true"
    mpath = "user/ubuntu/tmp/longitudinalBenchmarking/"
    resp1 = httr::PUT(paste0(uri_1,mpath,rdf_file,uri_2))
    httr::PUT(resp1$url, body=setNames(list(file=httr::upload_file(rdf_file)),rdf_file))
    resp1 = httr::PUT(paste0(uri_1,mpath,ts_file,uri_2))
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

launch_module <- function(buildingsRdf, timeseries){
    #buildingsRdf <- graph
    timeseriesObject<-lapply(timeseries, FUN=function(x){return(as.data.frame(x))})
    allBuildingSubjects <- get_all_buildings_list(buildingsRdf)

    for (buildingSubject in allBuildingSubjects){
      #buildingSubject <- allBuildingSubjects[1]
      tryCatch({
        start_time <- Sys.time()
        buildingId <- get_building_identifiers(buildingSubject)
        buildingId <- gsub("\\.",'',(gsub('#','',buildingId)))
        write(sprintf("Building: %s", buildingSubject),stderr())
        write(sprintf("reporter:status: START %s", buildingSubject),stderr())
        if (buildingId=="icaencatBuilding-00108"){
            save_data(buildingId, buildingsRdf, timeseriesObject)
        }
        if (already_executed(buildingId)){
            next
        }
        log_status(buildingId, "starting")
        tz <- get_tz_building(buildingsRdf, buildingSubject)
        buildingArea <- get_area_building(buildingsRdf, buildingSubject)

        for (et in 1:length(settings$EnergyTypes)){
          #et=2
          energyType <- names(settings$EnergyTypes)[et]
          deviceAggregator <- unlist(settings$EnergyTypes[et])
          buildingData <- get_device_aggregators_by_building(
            buildingsRdf,
            timeseriesObject, #If character, data directory is provided;
                              #if list of dataframes, the timeseries are provided.
            allowedBuildingSubjects=buildingSubject,
            allowedDeviceAggregators=c(deviceAggregator,"outdoorTemperature"),
            useEstimatedValues=F, ratioCorrection=F)

          if(!all(
              mapply(function(i){
                any(grepl(paste0("_",i, collapse="|"),
                  colnames(buildingData[[buildingSubject]]$df)))},
                unique(buildingData[[buildingSubject]]$metadata$measuredProperty)
              )
            )){next}

          write("* Reading and aligning the input data",stderr())
          write(sprintf("reporter:status: READ %s", buildingSubject),stderr())

          df <- data.frame(
            "time" = buildingData[[buildingSubject]]$df$time,
            "temperature" = buildingData[[buildingSubject]]$df[,paste0("outdoorTemperature",".AVG_Temperature")],
            "Qe" = buildingData[[buildingSubject]]$df[,
              paste0(deviceAggregator,".SUM_",
                     buildingData[[buildingSubject]]$metadata$measuredProperty[
                       buildingData[[buildingSubject]]$metadata$deviceAggregatorName==deviceAggregator
                     ][1])],
            "Qe_cost" = buildingData[[buildingSubject]]$df[,
              paste0(deviceAggregator,".SUM_EnergyCost")],
            "Qe_emissions" = buildingData[[buildingSubject]]$df[,
              paste0(deviceAggregator,".SUM_EnergyEmissions")],
            "Qe_price" = buildingData[[buildingSubject]]$df[,
              paste0(deviceAggregator,".AVG_",
                    buildingData[[buildingSubject]]$metadata$measuredProperty[
                      buildingData[[buildingSubject]]$metadata$deviceAggregatorName==deviceAggregator
                    ][1],"_EnergyPrice")],
            "Qe_emissionsFactor" = buildingData[[buildingSubject]]$df[,
              paste0(deviceAggregator,".AVG_",
                    buildingData[[buildingSubject]]$metadata$measuredProperty[
                      buildingData[[buildingSubject]]$metadata$deviceAggregatorName==deviceAggregator
                    ][1],"_EnergyEmissionsFactor")]
            )
          df <- df[min(which(is.finite(df$Qe))):(nrow(df)-min(which(is.finite(rev(df$Qe))))),]

          datePeriod <- as.duration(as.Date(max(df$time[is.finite(df$Qe)]), tz=tz) -
                                       as.Date(min(df$time[is.finite(df$Qe)]), tz=tz))
          datePeriodInBaselineYear <- tryCatch({
            sum(
              (year(df$time) %in% settings$BaselineYears) *
              hourly_timesteps(as.numeric(as.duration(detect_time_step(df$time)))/3600,"P1Y"),
              na.rm=T
            ) >=
            (8760 * settings$MinRatioBaseline * length(settings$BaselineYears))
          },error=function(e){FALSE})

          write("* Launcher modes: ",stderr())
          write(sprintf("reporter:status: LAUNCH %s", buildingSubject),stderr())

          launcherModes <-
          # Less than one year of data
            if(datePeriod < as.duration("P1Y")){
              c("NoModel")
          # Between one year and three years of data
            } else if (datePeriod < as.duration("P3Y") && datePeriodInBaselineYear){
              if(as.period(detect_time_step(df$time)) <= as.period("PT1H") ){
                c("HourlyBaselineModel")
              } else if (as.period(detect_time_step(df$time)) <= as.period("P1M") ){
                c("MonthlyBaselineModel")
              } else {
                c("NoModel")
              }
          # Yearly data frequency and more than 6 years
            } else if ( datePeriod >= as.duration("P6Y") &&
                        as.period(detect_time_step(df$time)) >= as.period("P1Y") ){
              c("YearlyDynamicModel")
          # More than three years of data and complete baseline year (> min ratio in settings %)
            } else if (datePeriodInBaselineYear){
              if(as.period(detect_time_step(df$time)) <= as.period("PT1H") ){
                c("HourlyBaselineModel", "HourlyDynamicModel")
              } else if (as.period(detect_time_step(df$time)) <= as.period("P1M") ){
                c("MonthlyBaselineModel", "MonthlyDynamicModel")
              } else {
                c("NoModel")
              }
          # Between one year and three years of data, no baseline year available, and monthly data frequency to beyond.
            } else {
              if(as.period(detect_time_step(df$time)) <= as.period("PT1H") ){
                c("HourlyDynamicModel")
              } else if (as.period(detect_time_step(df$time)) <= as.period("P1M") ){
                c("MonthlyDynamicModel")
              } else {
                c("NoModel")
              }
            }

          write(paste0("    ",paste(launcherModes,collapse=", ")),stderr())

          ####
          # Generate the indicators ----
          ####

          write("* Generating the real indicators",stderr())
          write(sprintf("reporter:status: REAL %s", buildingSubject),stderr())

          ## Indicators based on real data
          results <- generate_longitudinal_benchmarking_indicators(
            data = df,
            indicators = unlist(settings$PerformanceIndicators),
            indicatorsUnitsSubjects = settings$IndicatorsUnitsSubjects,
            energyComponent = "Total",
            energyType = energyType,
            frequencies = settings$Frequencies[
              as.period(settings$Frequencies)>=as.period(detect_time_step(df$time))],
            buildingId = buildingId,
            buildingSubject = buildingSubject,
            localTimeZone = tz,
            timeColumn = "time",
            consumptionColumn = "Qe",
            energyPriceColumn = "Qe_price",
            carbonEmissionsColumn = "Qe_emissionsFactor",
            buildingGrossFloorArea = buildingArea,
            outdoorTemperatureColumn = "temperature",
            heatingDegreeDays18Column = if(sum(colnames(df) %in% c("HDD18"))>0) {
              colnames(df)[colnames(df) %in% c("HDD18")]
              } else {NULL},
            coolingDegreeDays21Column = if(sum(colnames(df) %in% c("CDD21"))>0) {
              colnames(df)[colnames(df) %in% c("CDD21")]
            } else {NULL},
            # carbonEmissionsColumn = ,
            # energyPriceColumn = ,
            # dailyFixedPriceColumn =,
            modelName = NULL,
            modelId = NULL,
            modelLocation = NULL,
            modelTypeSubject = NULL,
            modelStorageInfrastructureSubject = NULL,
            estimateWhenAggregate = T,
            prevResults = NULL
          )

          # Indicators based on models
          for(modelName in launcherModes[(launcherModes %in% "HourlyDynamicModel")]){
            #modelName = "HourlyDynamicModel"
            modelType <- if(grepl("DynamicModel",modelName)){"DynamicModel"} else {"BaselineModel"}
            write(sprintf("* Generating the estimated indicators based on %s",modelName),stderr())
            write(sprintf("reporter:status: ESTIMATE %s", buildingSubject),stderr())
            # Evaluate the model, if needed, and predict the indicators
            call <- paste0(
              modelName, paste0("(df=df, identifier=paste(buildingId,deviceAggregator,sep='~'),
                                settings=settings, tz=tz, plots = T)"))
            model <- eval(parse(text=call))

            energyComponentsColumns <- c("Total"="Qe",
                                         "Baseload"="baseloadR",
                                         "Heating"="heatingR",
                                         "Cooling"="coolingR")
            baselineEnergyComponentsColumns <- c("Total"="predicted",
                                                 "Baseload"="baseload",
                                                 "Heating"="heating",
                                                 "Cooling"="cooling")

            ggplot(model$data) +
              geom_line(aes(time,baseload+cooling),col="blue",alpha=0.7) +
              geom_line(aes(time,baseload+heating),col="red",alpha=0.7) +
              geom_line(aes(time,baseload),col="black",alpha=0.7)
            ggplot(model$data) +
              geom_line(aes(time,baseloadR+coolingR),col="blue",alpha=0.7) +
              geom_line(aes(time,baseloadR+heatingR),col="red",alpha=0.7) +
              geom_line(aes(time,baseloadR),col="black",alpha=0.7)

            for (i in 1:length(energyComponentsColumns)){
              if(!(energyComponentsColumns[i] %in% colnames(model$data))){next}
              results <- generate_longitudinal_benchmarking_indicators(
                data = model$data,
                indicators = unlist(settings$TrendIndicators),
                indicatorsUnitsSubjects = settings$IndicatorsUnitsSubjects,
                energyComponent = names(energyComponentsColumns)[i],
                energyType = energyType,
                frequencies = settings$Frequencies[
                  as.period(settings$Frequencies) >= as.period(detect_time_step(model$data$time))],
                buildingId = buildingId,
                buildingSubject = buildingSubject,
                localTimeZone = tz,
                timeColumn = "time",
                consumptionColumn = energyComponentsColumns[i],
                energyPriceColumn = "Qe_price",
                carbonEmissionsColumn = "Qe_emissionsFactor",
                baselineConsumptionColumn = baselineEnergyComponentsColumns[i],
                buildingGrossFloorArea = buildingArea,
                # outdoorTemperatureColumn = ,
                # carbonEmissionsColumn = ,
                # energyPriceColumn = ,
                # dailyFixedPriceColumn =,
                modelName = model$name,
                modelId = model$id,
                modelLocation = model$subject,
                modelTypeSubject = sprintf("bigg:MODELTYPE-%s",modelType),
                modelStorageInfrastructureSubject = "bigg:MODELINFRA-MLFlow",
                estimateWhenAggregate = T,
                prevResults = results
              )
            }
          }
          # Write the RDF files
          write(sprintf("reporter:status: SAVE %s", buildingSubject),stderr())
          dir.create(settings$OutputDataDirectory)
          write_rdf(object = results$results_rdf,
                    file = paste(settings$OutputDataDirectory,
                                 sprintf("%s.ttl", digest(
                                   paste0(buildingSubject, unlist(settings$Indicators),
                                          energyType, collapse="~"),
                                   algo = "sha256", serialize = T)), sep="/"))
          # Write the JSON files (time series)
          for(i in 1:length(results$results_ts)){
            write(jsonlite::toJSON(
              setNames(list(results$results_ts[[i]]),names(results$results_ts)[i]),
              dataframe = "rows",na = "null"),
              file=paste(settings$OutputDataDirectory,
                         sprintf("%s.json",names(results$results_ts)[i]),
                         sep="/") )
          }
          # load files to neo4j
          user <- settings$userMap[strsplit(buildingSubject, "#", fixed=T)[[1]][1]][[1]]
          pyos <- import("os")
          load_ttl <- import("load_ttl")
          load_ttl$load_ttl_to_neo4j(settings$OutputDataDirectory, user)
          unlink(settings$OutputDataDirectory, recursive = TRUE)
          end_time <- Sys.time()
          diff_time = end_time - start_time
          write(paste0("Building calculated in ",diff_time," and using ",mem_used()),  file=stderr())

          rm(results)
          rm(buildingData)
          rm(df)
          gc(reset=T)
          s = sort( -sapply(ls(),function(x){object.size(get(x))}))
          write(names(s[1:10]),file=stderr())
          write(paste0("After cleaning ", mem_used()),  file=stderr())
          log_status(buildingId, "success")

        }
      }, error=function(e){
        write(sprintf("###### Error ###### %s",e),stderr())
        write(sprintf("reporter:status: ERROR %s", buildingSubject), stderr())
        if(buildingSubject %in% settings$storeFailed){
            save_data(buildingId, buildingsRdf, timeseriesObject)
        }
        end_time <- Sys.time()
        diff_time = end_time - start_time
        write(paste0("Building calculated in ",diff_time," and using ",mem_used()),  file=stderr())
        rm(results)
        rm(buildingData)
        rm(df)
        gc(reset=T)
        s = sort( -sapply(ls(),function(x){object.size(get(x))}))
        write(names(s[1:10]),file=stderr())
        write(paste0("After cleaning ", mem_used()),  file=stderr())
        log_status(buildingId, "error")

      })
    }
}
