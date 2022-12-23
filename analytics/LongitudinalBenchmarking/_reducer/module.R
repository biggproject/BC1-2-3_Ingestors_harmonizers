library(qgam)
imports = c("biggr", "data.table","jsonlite","ggplot2", "lubridate",
"gridExtra", "plotly", "padr", "htmlwidgets", "carrier","mlflow","fs",
"tidyr","digest")
for(pack in imports){suppressMessages(suppressWarnings(library(pack, character.only = TRUE)))}

source(paste0(SCRIPT_PATH,"/HourlyDynamicModel.R"))
source(paste0(SCRIPT_PATH,"/HourlyBaselineModel.R"))
source(paste0(SCRIPT_PATH,"/MonthlyDynamicModel.R"))
source(paste0(SCRIPT_PATH,"/MonthlyBaselineModel.R"))
source(paste0(SCRIPT_PATH,"/YearlyDynamicModel.R"))

settings <- fromJSON(paste0(SCRIPT_PATH,"/Settings.json"))
settings <- append(settings,fromJSON(paste0(SCRIPT_PATH,"/config.json")))

launch_module <- function(buildingsRdf, timeseries){
    #buildingsRdf <- graph
    allBuildingSubjects <- get_all_buildings_list(buildingsRdf)

    for (buildingSubject in allBuildingSubjects){
      #buildingSubject <- allBuildingSubjects[1]
      tryCatch({

        buildingId <- get_building_identifiers(buildingSubject)

        write("",stderr())
        write("###################################",stderr())
        write("#### Longitudinal benchmarking ####",stderr())
        write("###################################",stderr())
        write(sprintf("   Building: %s", buildingSubject),stderr())
        write("",stderr())


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

          write("",stderr())
          write("* Reading and aligning the input data",stderr())

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

            # Evaluate the model, if needed, and predict the indicators
            call <- paste0(
              modelName, paste0("(df=df, identifier=paste(buildingId,deviceAggregator,sep='~'),
                                settings=settings, plots = T)"))
            model <- eval(parse(text=call))

            energyComponentsColumns <- c("Total"="Qe",
                                         "Baseload"="baseloadR",
                                         "Heating"="heatingR",
                                         "Cooling"="coolingR")
            baselineEnergyComponentsColumns <- c("Total"="predicted",
                                                 "Baseload"="baseload",
                                                 "Heating"="heating",
                                                 "Cooling"="cooling")

            # ggplot(model$data) +
            #   geom_line(aes(time,baseload+cooling),col="blue",alpha=0.7) +
            #   geom_line(aes(time,baseload+heating),col="red",alpha=0.7) +
            #   geom_line(aes(time,baseload),col="black",alpha=0.7)
            # ggplot(model$data) +
            #   geom_line(aes(time,baseloadR+coolingR),col="blue",alpha=0.7) +
            #   geom_line(aes(time,baseloadR+heatingR),col="red",alpha=0.7) +
            #   geom_line(aes(time,baseloadR),col="black",alpha=0.7)

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
          gc(reset=T)
        }
      }, error=function(e){write(sprintf("###### Error ###### %s",e),stderr())})
    }
}