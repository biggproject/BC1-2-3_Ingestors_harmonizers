EEMs_assessment <- function(buildingSubject, buildingsRdf, timeseriesObject, settings, libraryPath="", updateHadoopStatus=F){
  
  library(biggr)
  
  source(paste(libraryPath,"HourlyMeasurementAndVerificationModel.R", sep="/"))
  source(paste(libraryPath,"YearlyMeasurementAndVerificationModel.R", sep="/"))
  
  dir.create(settings$OutputDataDirectory,showWarnings = F)
  

  buildingId <- get_building_identifiers(buildingSubject)

  write("",stderr())
  write("#############################################",stderr())
  write("#### Measurement and verification of EEM ####",stderr())
  write("#############################################",stderr())
  write(sprintf("   Building: %s", buildingSubject),stderr())
  write("",stderr())
    
  tz <- get_tz_building(buildingsRdf, buildingSubject)
  buildingArea <- get_area_building(buildingsRdf, buildingSubject)
    
  building_results <- list()
    
  for (j in 1:length(settings$MeasuredPropertiesDeviceAggregators)){
      #j=1
    results<-NULL
    tryCatch({
      deviceAggregator <- names(settings$MeasuredPropertiesDeviceAggregators)[[j]]
      write(sprintf("* Device Aggregator name: %s", deviceAggregator),stderr())
      measuredPropertyOutput <- settings$MeasuredPropertiesDeviceAggregators[[j]]
      measuredPropertiesInput <- c(settings$MeasuredPropertiesDeviceAggregators[[j]],"Temperature")
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: RUNNING %s %s", buildingSubject, measuredPropertyOutput),stderr())
      }
      buildingData <- get_device_aggregators(
        buildingsRdf, 
        timeseriesObject, #If character, data directory is provided; 
        #if list of dataframes, the timeseries are provided.
        allowedBuildingSubjects=buildingSubject, 
        allowedMeasuredProperties=measuredPropertiesInput,
        useEstimatedValues=F, ratioCorrection=T, containsEEMs=T,
        alignGreaterThanHourlyFrequencyToYearly=T)
      
      # If the measured property is not available in buildingData, deprecate this case
      if(!any(mapply(function(x) regexpr(measuredPropertyOutput,x),
                     colnames(buildingData[[buildingSubject]]$df))>1)) {
        write("* No data related with the measuredProperty",stderr())
        write(paste(unique(
          paste(buildingData[[buildingSubject]]$metadata$deviceAggregatorName,
                buildingData[[buildingSubject]]$metadata$deviceAggregatorFormula,sep=": ")),collapse=", "),stderr())
        next
      }
      
      write("",stderr())
      write("* Reading and aligning the input data",stderr())
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: READING DATA %s %s", buildingSubject, measuredPropertyOutput),stderr())
      }
      # Outdoor temperature data in degree days format
      if(any(grepl("HDD|CDD",colnames(buildingData[[buildingSubject]]$df),fixed=F))){
        df <- cbind(
          data.frame(
            "time" = buildingData[[buildingSubject]]$df$time,
            "Qe" = buildingData[[buildingSubject]]$df[,
                                                      paste0(deviceAggregator,".SUM_",measuredPropertyOutput)],
            "Qe_cost" = buildingData[[buildingSubject]]$df[,
                                                           paste0(deviceAggregator,".SUM_EnergyCost")],
            "Qe_emissions" = buildingData[[buildingSubject]]$df[,
                                                                paste0(deviceAggregator,".SUM_EnergyEmissions")],
            "Qe_price" = buildingData[[buildingSubject]]$df[,
                                                            paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyPrice")],
            "Qe_emissionsFactor" = buildingData[[buildingSubject]]$df[,
                                                                      paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyEmissionsFactor")]
          ),
          setNames(buildingData[[buildingSubject]]$df[,grepl("HDD|CDD",colnames(buildingData[[buildingSubject]]$df),fixed=F)],
                   gsub("outdoorTemperature.|_Temperature","",colnames(buildingData[[buildingSubject]]$df)[
                     grepl("HDD|CDD",colnames(buildingData[[buildingSubject]]$df),fixed=F)]))
        )
        # Outdoor temperature data in raw format
      } else {
        df <- data.frame(
          "time" = buildingData[[buildingSubject]]$df$time,
          "temperature" = buildingData[[buildingSubject]]$df[,paste0("outdoorTemperature",".AVG_Temperature")],
          "Qe" = buildingData[[buildingSubject]]$df[,
                                                    paste0(deviceAggregator,".SUM_",measuredPropertyOutput)],
          "Qe_cost" = buildingData[[buildingSubject]]$df[,
                                                         paste0(deviceAggregator,".SUM_EnergyCost")],
          "Qe_emissions" = buildingData[[buildingSubject]]$df[,
                                                              paste0(deviceAggregator,".SUM_EnergyEmissions")],
          "Qe_price" = buildingData[[buildingSubject]]$df[,
                                                          paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyPrice")],
          "Qe_emissionsFactor" = buildingData[[buildingSubject]]$df[,
                                                                    paste0(deviceAggregator,".AVG_",measuredPropertyOutput,"_EnergyEmissionsFactor")]
        )
      }
      
      # Filter dataframe between the minimum and maximum time with the consumption data
      df <- df[min(which(is.finite(df$Qe))):(nrow(df)+1-min(which(is.finite(rev(df$Qe))))),]
      
      # Add the EEMs in buildingData
      eems <- get_building_eems(buildingsRdf,buildingSubject)
      eems_details <- get_eem_details(buildingsRdf,eems$eemSubject)
      eems_details <- eems_details %>% left_join(eems,by="eemSubject")
      eems_details$Date <- with_tz(eems_details$Date, tz)
      eems_details <- eems_details[order(eems_details$Date,decreasing = F),]
      eems_details$AffectationShare <- ifelse(is.finite(eems_details$AffectationShare),
                                              eems_details$AffectationShare, settings$EEMDefaultShareOfAffectedElement)
      eems_details$Lifespan <- get_eem_lifespan(lifespans = settings$EEMLifespan,
                                                eemTypes = eems_details$Type)
      eems_details$DiscountRate <- settings$EEMDiscountRate
      eems_details$TrainingDays <- as.numeric(difftime(eems_details$Date,min(df$time),units = "days"))
      eems_details$TrainingDataTimesteps <- sum(df$time >= min(df$time) & df$time < eems_details$Date)
      eems_details$DateNext <- lead(eems_details$Date,1)#[-nrow(eems_details)],
      #as.Date(with_tz(max(df$time),"UTC")))
      eems_details$NextMeasureDays <- as.numeric(difftime(eems_details$DateNext,eems_details$Date,units = "days"))
      eems_details$ValidationDays <- as.numeric(difftime(max(df$time)+as.period(detect_time_step(df$time))-1,eems_details$Date,
                                                         units = "days"))
      eems_details_unfiltered <- eems_details
      eems_details <- eems_details %>% filter(
        (eems_details$TrainingDays > settings$EEMAssessmentConditions$MinimumDaysBeforeEEM) &
          (eems_details$TrainingDataTimesteps > settings$EEMAssessmentConditions$MinimumTimestepsBeforeEEM) &
          (eems_details$ValidationDays > settings$EEMAssessmentConditions$MinimumDaysAfterEEM)
      )
      if(nrow(eems_details)>0){
        eems_details$eemProjectId <- 1
        eemProjectId <- 1
        for(g in 1:nrow(eems_details)){
          if(
            (is.na(eems_details$NextMeasureDays[g]) | 
             (eems_details$NextMeasureDays[g] > settings$EEMAssessmentConditions$MinimumDaysAfterEEM)
            ) & (eems_details$ValidationDays[g] > settings$EEMAssessmentConditions$MinimumDaysAfterEEM)
          ){
            eems_details$eemProjectId[g] <- eemProjectId
            eemProjectId <- eemProjectId + 1
          } else {
            eems_details$eemProjectId[g] <- eemProjectId
          }
        }
        for(eemProjectId in unique(eems_details$eemProjectId)) {
          eems_in_project <- eems_details[eems_details$eemProjectId==eemProjectId,]
          if(as.period(detect_time_step(df$time))<=as.period("PT1H")){
            df[,paste0("eemProject.",eemProjectId)] <- 
              ifelse(df$time<min(eems_in_project$Date), 0,
                     ifelse(df$time>=max(eems_in_project$Date) & 
                              (df$time<tail(eems_in_project$DateNext,1) | 
                                 is.na(tail(eems_in_project$DateNext,1))),
                            1, NA)
              )
            df[,paste0("eemProject.",eemProjectId)] <- 
              ifelse(as.numeric(df$time)<=(max(eems_in_project$Date)+settings$EEMAssessmentConditions$MaximumDaysAfterEEM*24*60*60),
                     df[,paste0("eemProject.",eemProjectId)], NA)
          } else {
            df_aux <- data.frame("time"= seq(min(df$time), max(df$time) + as.period(detect_time_step(df$time)), by="hours"))
            df_aux[,paste0("eemProject.",eemProjectId)] <- ifelse(df_aux$time<min(eems_in_project$Date),0,
                                                                  ifelse(df_aux$time>=max(eems_in_project$Date),1,NA))
            df_aux$agg <- cumsum(df_aux$time %in% df$time)
            df_aux <- df_aux %>% group_by(agg) %>% 
              summarise(time=first(time),across(paste0("eemProject.",eemProjectId),mean)) %>% 
              ungroup() %>% select(-agg)
            df <- df %>% left_join(df_aux, by="time")
          }
        }
        
      } else {
        write("* Any EEM project overlap the minimum range considered in settings. The savings can't be estimated. ",stderr())
        #write(jsonlite::toJSON(as.list(eems_details_unfiltered),pretty = T,auto_unbox = T),stderr())
        next
      }
      
      modelName <- if(detect_time_step(df$time) <= as.duration("PT1H")){
        "HourlyMeasurementAndVerificationModel"
      } else {
        "YearlyMeasurementAndVerificationModel"
      }
      
      write(paste0("    ",paste(modelName,collapse=", ")),stderr())
      
      # Indicators based on models
      write(sprintf("* Estimating the savings of EEMs based on %s",modelName),stderr())
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: ESTIMATING SAVINGS: %s, %s", buildingSubject, measuredPropertyOutput),stderr())
      }

      # Evaluate the model, if needed, and predict the indicators
      if(modelName %in% c("HourlyMeasurementAndVerificationModel")){
        call <- paste0(
            modelName, paste0("(df = df, buildingId = buildingId, buildingSubject = buildingSubject,
                                identifier = paste(buildingId,deviceAggregator,'EEM',sep='~'),
                                settings = settings, tz = tz,
                                plots = as.logical(settings$MLFlow$IncludeArtifacts * settings$MLFlow$StoreModels),
                                eems = eems_details, measuredProperty = measuredPropertyOutput,
                                buildingGrossFloorArea = buildingArea, results = results, updateHadoopStatus=updateHadoopStatus)"))
        building_results[[j]] <- eval(parse(text=call))
      } else {
        building_results[[j]] <- "Unimplemented model"
      }
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: SUCCESS %s %s", buildingSubject, measuredPropertyOutput),stderr())
      }
    }, error=function(e){
      write(sprintf("###### Error ###### %s",e),stderr())
      if(updateHadoopStatus==T){
        write(sprintf("reporter:status: ERROR %s %s", buildingSubject, measuredPropertyOutput),stderr())
      }
      building_results[[j]] <- as.character(e)
    })
  }
  return(building_results)
}
