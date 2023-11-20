MonthlyMeasurementAndVerificationModel <- function(df, buildingId, buildingSubject, identifier, settings, tz, eems, buildingGrossFloorArea, 
                                                   measuredProperty, ignoredDates, plots=T, results=list()){
  
  suppressMessages(suppressWarnings(library(ggplot2)))
  suppressMessages(suppressWarnings(library(htmlwidgets)))
  suppressMessages(suppressWarnings(library(plotly)))
  suppressMessages(suppressWarnings(library(carrier)))
  suppressMessages(suppressWarnings(library(mlflow)))
  suppressMessages(suppressWarnings(library(httr)))
  suppressMessages(suppressWarnings(library(jsonlite)))
  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(lubridate)))
  
  modelName <- "MonthlyMeasurementAndVerificationModel"
  
  dir.create(settings$OutputDataDirectory,F)
  if(plots){
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  
  # MLFlow configuration, fetch the experimentId and last registered model, if available.
  Sys.setenv(MLFLOW_PYTHON_BIN=settings$PYTHON3_BIN)
  Sys.setenv(MLFLOW_TRACKING_URI=settings$MLFlow$TrackingUri)
  Sys.setenv(MLFLOW_VERBOSE=FALSE)
  Sys.setenv(MLFLOW_BIN=settings$MLFlow$MLFLOW_BIN)
  
  harmonised_results <- results
  
  ####
  # TRAIN AND PREDICT ----
  ####
  
  write("## Training process is starting",stderr())
  
  if(plots){
    ts_p <- ggplot( 
      reshape2::melt( 
        df %>% 
          select(time, Qe, HDD21, starts_with("eemProject")) %>% 
          suppressMessages(pad())
        ,"time") %>% mutate(
          variable = factor(variable,
                            levels=unique(variable),
                            labels=mapply(function(x){
                              if(x=="Qe"){ 'plain(consumption~(kWh))'
                              } else if(x=="HDD21"){'plain(outdoor~temperature)~plain((degree*C))'
                              } else {paste0("plain(",gsub("eemProject.","Energy~Efficiency~Measures~project~",x,fixed = T),
                                             ")~(0==training~1==evaluation)")}},unique(variable)))
        )
    ) + 
      geom_line(aes(time,value)) +
      ylab("") +
      facet_wrap(~variable, scales = "free_y", ncol=1, labeller = label_parsed) +
      theme_bw() + theme(axis.text.x = element_text(hjust=1))
    ggsave(paste(settings$OutputDataDirectory,"plots","raw_consumption_temperature.pdf",sep="/"),
           ts_p, width=7, height=3.5+sum(colnames(df) %>% startsWith("eemProject")))
  }
  
  ####
  # Calendar features and filtering of holidays and special periods ----
  ####
  
  # Detect holidays
  write("* Detecting the holidays",stderr())
  # function in biggr -> preparation.R
  holidaysDates <- holidaysNAGER(
    y = unique(format(as.Date(df$time, format="%d/%m/%Y"),"%Y")), # "2023"
    country = "ES",
    region="CT"
  )
  
  # Add the calendar components
  df <- df %>% calendar_components( # biggr, transformation.r line 425
    localTimeZone = tz,
    holidays = holidaysDates,
    inplace=T
  )
  
  if(plots){
    h <- ggplot(
      df %>% select(time, Qe, isHolidays) %>% 
        group_by(date=as.Date(time,tz=tz)) %>% 
        summarise(Qe=sum(Qe),isHolidays=any(as.logical(isHolidays)))
    ) + 
      geom_line(aes(date,Qe),size=0.1,alpha=0.5) +
      geom_point(aes(date,Qe,col=isHolidays), size=0.1) +
      scale_color_manual("",values=c("TRUE"="red","FALSE"="black"),labels=c("TRUE"="Holiday","FALSE"="Not holiday")) +
      theme_bw() + theme(axis.text.x = element_text(hjust=1), legend.position = "top") +
      ylab("consumption (kWh)") + xlab("time")
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"holidays.pdf",sep="/"),
           h, width=7, height=3)
  }
  
  ####
  # Outliers detection ----
  ####
  
  write("* Detecting the outliers",stderr())
  if(all(c("value","window") %in% colnames(df)))
    df <- df %>% select(-value, -window)
  df <- 
    df %>%
    select(!(contains("outliers") | contains("upperPredCalendarModel") | 
               contains("lowerPredCalendarModel"))) %>%
    left_join(
      detect_ts_calendar_model_outliers(
        data = df,
        localTimeColumn = "localtime",
        valueColumn = "Qe", 
        calendarFeatures = settings$DataCleaning$OutliersDetection$calendarFeatures$PT1H,
        mode = settings$DataCleaning$OutliersDetection$mode,
        upperModelPercentile = settings$DataCleaning$OutliersDetection$upperModelPercentile,
        lowerModelPercentile = settings$DataCleaning$OutliersDetection$lowerModelPercentile,
        upperPercentualThreshold = settings$DataCleaning$OutliersDetection$upperPercentualThreshold,
        lowerPercentualThreshold = settings$DataCleaning$OutliersDetection$lowerPercentualThreshold,
        window = settings$DataCleaning$OutliersDetection$window$PT1H,
        outputPredictors = T,
        holidaysCalendar = holidaysDates,
        daysThatAreOutliers = ignoredDates,
        logValueColumn = F,
        autoDetectProfiled = T),
      by = "localtime"
    )
  abnormalDays <- sort(df %>% 
                         group_by(date) %>% summarise(out=sum(outliers)) %>%
                         filter(out>0) %>% select(date) %>% unlist(.) %>% as.Date(.))
  df$abnormalDay <- df$date %in% abnormalDays
  
  # IRENE just before "for"
  if(plots){
    g <- ggplot(df %>% select(localtime, Qe, outliers) %>% 
                  group_by(date=as.Date(localtime,tz=tz)) %>% 
                  summarise(Qe=sum(Qe),outliers=any(as.logical(outliers))) ) +
      geom_line(aes(date,Qe),size=0.1,alpha=0.5) +
      theme_bw() + theme(axis.text.x = element_text(hjust=1), legend.position = "top") +
      ylab("consumption (kWh)") + xlab("time")
    if(!all(is.na(df$outliers) | df$outliers==F)){
      g <- g + geom_point(aes(date,Qe,col=outliers),size=0.1) + 
        scale_color_manual("",values=c("TRUE"="red","FALSE"="black"),labels=c("TRUE"="Day with outliers","FALSE"="Normal day"))
    }
    ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"outliers_plot.pdf",sep="/"),
           g, width=7, height=3)
  }
  
  for (eemProject in colnames(df)[startsWith(colnames(df),"eemProject")]){
    
    # eemProject="eemProject.1"
    
    identifier_case <- paste0(identifier,"~",eemProject)
    df_by_eem <- df[is.finite(df[,eemProject]),]
    
    # Assess the EEM only when a minimum number of days with non-outlier data are available inside the 
    # min-max range of days after the EEM (all of them, defined in settings/EEMAssessmentConditions)
    if(sum(df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==1) <= 
       hourly_timesteps(settings$EEMAssessmentConditions$MinDaysWithoutOutliers*24, detect_time_step(df_by_eem$time))) {
      
      write(sprintf(
        "* %s wasn't assessed because there are not enough valid days to make the estimation", eemProject) ,stderr())
      
    } else {
      
      ###
      # Setting the model parameters and transformations to be done during the model training ----
      ###
      
      write("* Setting model parameters and transformations",stderr())
      
      generalTransformationSentences <- list(
        # Cast to numeric of the holidays feature
        "holidays" = "ifelse(isHolidays==T,2,1)",
        
        # Fill some gaps in the outdoor HDD21 time series.
        "HDD21" = "na.locf(
                              na.locf(
                                na.approx(HDD21,na.rm = F),
                                fromLast = T, na.rm = F
                              ),
                              na.rm=F)"
      )
      if(length(unique(df$isHolidays))==1){
        generalTransformationSentences <- generalTransformationSentences["HDD21"]
      }
      
      trControl <- trainControl(method="none")
      
      
      ###
      # Model training ----
      ###
      
      write("* Training of the model and loading process to MLFlow",stderr())
      
      
      model <- function(df, generalTransformationSentences){
        HC_model <- function(df, ...) {
          args <- list(...)
          if(length(unique(df$isHolidays))==1){ # If there are no holidays
            train( ## S3 method for class 'recipe' (from train (caret package) documentation)
              Qe ~ monthInt + HDD21 + CDD21, # columns from df table
              data=df,
              method = RandomForest(NULL),
              trControl = trControl,
              maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
              minPredictionValue = 0,
              transformationSentences = args$transformationSentences)
          } else {
            train(
              Qe ~ monthInt + HDD21 + CDD21 + holidays + holidaysPerMonth,
              data=df,
              method = RandomForest(NULL),
              trControl = trControl,
              maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
              minPredictionValue = 0,
              transformationSentences = args$transformationSentences)
          }
        }
        mod <- HC_model(df = df,
                        transformationSentences = generalTransformationSentences)
        return(mod)
      }
      
      trained_models <- list(
        "baseline" = model(
          df = df_by_eem[df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==0,],
          generalTransformationSentences = generalTransformationSentences),
        "after_eem" = model(
          df = df_by_eem[df_by_eem$outliers==F & !is.na(df_by_eem$outliers) & df_by_eem[,eemProject]==1,],
          generalTransformationSentences = generalTransformationSentences)
      )
      
      # Generate the predictor objects
      predictor <- carrier::crate(function(x, baseline, forceGlobalInputFeatures = NULL, predictionIntervals = F){
        mod <- !!trained_models
        biggr::predict.train(
          object = mod[[if(baseline){"baseline"}else{"after_eem"}]],
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          predictionIntervals = predictionIntervals
        )
      })
      
      counterfactual <- predictor(x = df_by_eem[df_by_eem[,eemProject] %in% c(0,1),],
                                  baseline = TRUE)
      counterfactual <- data.frame("time" = df_by_eem$time,
                                   "real" = df_by_eem$Qe,
                                   "predicted" = counterfactual,
                                   "eemProject" = df_by_eem[, eemProject],
                                   "outliers" = df_by_eem$outliers,
                                   "Qe_price" = df_by_eem$Qe_price,
                                   "Qe_emissionsFactor" = df_by_eem$Qe_emissionsFactor)
      
      # after_eem <- predictor(x = df_by_eem[df_by_eem[,eemProject]==1 & df_by_eem$outliers==F & !is.na(df_by_eem$outliers),],
                            # baseline = FALSE)
      
      # after_eem$time <- with_tz(after_eem$time,"UTC")
      # counterfactual$time <- with_tz(counterfactual$time,"UTC")
      # df_by_eem$time <- with_tz(df_by_eem$time,"UTC")
      #results <- df_by_eem[df_by_eem[,eemProject]==1 & !is.na(df_by_eem[,eemProject]),] %>% mutate("total"=counterfactual)
      #results <- df_by_eem %>% mutate("total"=counterfactual) %>% filter(across(any(eemProject), ~identical(.x,1) + !is.na(.x))) 
      
      results <- counterfactual %>% filter(eemProject == 1 & !is.na(eemProject))
      
      # after_eem$time <- with_tz(after_eem$time,tz)
      # counterfactual$time <- with_tz(counterfactual$time,tz)
      # df_by_eem$time <- with_tz(df_by_eem$time,tz)
      
      # Plot the energy savings
      if(plots){
        time_aggregate_results <- function(results, timeColumn, outputFrequency, funAggregation, estimate=F){
          times <- results[,timeColumn]
          results[,timeColumn] <- NULL
          results[,"time"] <- times
          cols <- colnames(results)[colnames(results) != "time"]
          results <- results %>% {
            if(is.null(outputFrequency)){
              group_by(.,
                       time = first(time)
              )
            } else {
              group_by(.,
                       time = lubridate::floor_date(time, lubridate::period(outputFrequency),
                                                    week_start = getOption("lubridate.week.start", 1)))
            }} %>%
            summarise(
              across(all_of(cols), function(value){
                if(funAggregation=="SUM"){
                  if(estimate){
                    mean(value,na.rm=T)*length(value)
                  } else {
                    sum(value,na.rm=T)
                  }
                } else if(funAggregation=="MEAN"){
                  mean(value,na.rm=T)
                }
              })
            ) %>%
            ungroup()
          return(results)
        }
        results__ <- data.frame(
          "predicted" = -results[,"real"] + results[,"predicted"],
          "time" = results$time)
        results__ <- time_aggregate_results(results__, timeColumn = "time",
                                            outputFrequency = "P1M", funAggregation = "SUM",
                                            estimate=T)
        g <- ggplot() +
          labs(y=expression(atop(plain("monthly consumption savings (kWh)"),
                                 atop(plain("negative means more consumption versus counterfactual"))))) +
          geom_line(data=results__, aes(time, predicted), col="black") +
          geom_point(data=results__, aes(time, predicted), col="black") + 
          geom_hline(aes(yintercept=0), size=0.3, linetype = "dashed") + 
          theme_bw()
        ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"savings.pdf",sep="/"),
               g, width=7, height=4)
      }
      
      # Plot the predicted time series with both models
      if(plots){
        p <- ggplot(counterfactual %>% pad(.,by = "time")) + 
          geom_line(aes(time,real), col="black") + 
          geom_line(aes(time,predicted),col="red",alpha=0.5) +
          ylim(c(0,max(counterfactual$predicted,na.rm=T)*1.1)) +
          geom_area(data=
                      df %>% 
                      filter( time >= min(counterfactual$time) & time<=max(counterfactual$time) ) %>%
                      select(time, matches(eemProject)) %>%
                      rename(all_of(c(time="time",eem=eemProject))),
                    aes(time, eem * max(c(counterfactual$predicted),1.1, na.rm=T)),
                    col="blue",size=0.1,alpha=0.2) +
          theme_bw() + xlab("time") + ylab("consumption (kWh)")
        
        ggsave(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"counterfactual.pdf",sep="/"), 
               p, width=7, height=3)
        
        eemDate <- min((df %>% 
                          filter( time >= min(counterfactual$time) & time<=max(counterfactual$time) ) %>%
                          select(time, matches(eemProject)) %>%
                          rename(all_of(c(time="time",eem=eemProject))) %>% filter(eem==1))$time)
      }
      
      
      ###
      ### Load to MLFlow if needed ----
      ###
      
      if(settings$MLFlow$StoreModels){
        
        experimentId <- tryCatch(
          mlflow_create_experiment(identifier_case),
          error = function(e){
            experiment <- mlflow_get_experiment(name=identifier_case)
            if (experiment$lifecycle_stage!="active") mlflow_restore_experiment(experiment$experiment_id)
            experiment$experiment_id
          }
        )
        
        with(
          mlflow_start_run(experiment_id = experimentId), {
            
            # Generate the MLflow instance
            mlflow_log_param("Model",modelName)
            
            # Store the params
            settings$FinalModel <- list("Parameters" = list())
            for (i in 1:ncol(trained_models[["baseline"]]$bestTune)){
              settings$FinalModel$Parameters[[colnames(trained_models[["baseline"]]$bestTune)[i]]] <-
                as.character(trained_models[["baseline"]]$bestTune[1,i])
              mlflow_log_param(paste0("Baseline - ",colnames(trained_models[["baseline"]]$bestTune)[i]),as.character(trained_models[["baseline"]]$bestTune[1,i]))
            }
            mlflow_log_param("MinTrainingDatetime",parsedate::format_iso_8601(min(df_by_eem$time)))
            settings$FinalModel$Parameters[["MinTrainingDatetime"]] <- parsedate::format_iso_8601(min(df_by_eem$time))
            mlflow_log_param("MaxTrainingDatetime",parsedate::format_iso_8601(max(df_by_eem$time)))
            settings$FinalModel$Parameters[["MaxTrainingDatetime"]] <- parsedate::format_iso_8601(max(df_by_eem$time))
            
            # Store the errors
            MAEmetric <- MAE(counterfactual$real[counterfactual$eemProject==0 & counterfactual$outliers==F & !is.na(counterfactual$outliers)],
                             counterfactual$predicted[counterfactual$eemProject==0 & counterfactual$outliers==F & !is.na(counterfactual$outliers)],na.rm = T)
            RMSEmetric <- RMSE(counterfactual$real[counterfactual$eemProject==0 & counterfactual$outliers==F & !is.na(counterfactual$outliers)],
                               counterfactual$predicted[counterfactual$eemProject==0 & counterfactual$outliers==F & !is.na(counterfactual$outliers)],na.rm = T)
            CVRMSEmetric <- RMSEmetric / mean(counterfactual$predicted[counterfactual$eemProject==0 & counterfactual$outliers==F & !is.na(counterfactual$outliers)],na.rm=T)
            r2metric <- cor(counterfactual$real[counterfactual$eemProject==0 & counterfactual$outliers==F & !is.na(counterfactual$outliers)],
                            counterfactual$predicted[counterfactual$eemProject==0 & counterfactual$outliers==F & !is.na(counterfactual$outliers)],
                            use = "na.or.complete")^2
            settings$FinalModel$Metrics <- list(
              "MAE" = MAEmetric,
              "RMSE" = RMSEmetric,
              "CVRMSE" = CVRMSEmetric,
              "r2" = r2metric)
            mlflow_log_metric("MAE", MAEmetric)
            mlflow_log_metric("RMSE", RMSEmetric)
            mlflow_log_metric("CVRMSE", CVRMSEmetric)
            mlflow_log_metric("r2", r2metric)
            
            # Store the artifacts
            if(plots){
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                        "raw_consumption_temperature.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                        "counterfactual.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                        "holidays.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                        "outliers_plot.pdf",sep="/"))
              mlflow_log_artifact(paste(settings$OutputDataDirectory,"plots",
                                        "savings.pdf",sep="/"))
            }
            
            # Store the model
            mlflow_log_model(predictor,"model")
            
            # Register the model
            tryCatch(mlflow_create_registered_model(paste0(identifier_case,"~",modelName)),error=function(e){})
            modelId <- mlflow_get_run()$run_uuid[1]
            modelSubject <- sprintf("runs:/%s/model",modelId)
            registeredModelName <- paste0(identifier_case,"~",modelName)
            mlflow_create_model_version(
              name = registeredModelName, 
              source = modelSubject,
              run_link = modelSubject,
              description= toJSON(settings,auto_unbox = T)
            )
            mlflow_log_param("modelId",modelId)
            mlflow_log_param("modelSubject",modelSubject)
            mlflow_log_param("registeredModelName",registeredModelName)
            
            settings$FinalModel <- NULL
          })
      } else {
        modelId <- NULL
        modelSubject <- NULL
      }
      
      for (measuredPropertyComponent in c("Total")){
        measuredPropertyComponent <- "Total"
        if(!any(eems[eems$eemProjectId==strsplit(eemProject,"\\.")[[1]][2],
                     paste0("MeasuredPropertyComponent_",measuredPropertyComponent)])){
          next
        }
        harmonised_results <- generate_eem_assessment_indicators(
          data = results, 
          indicatorsAggregatableByTime = unlist(settings$Indicators[c("Energy","Cost","Emissions")]), 
          indicatorsNonAggregatableByTime = if(measuredPropertyComponent=="Total"){settings$Indicators$NotAggregableByTime}else{NULL},
          measuredProperty = measuredProperty, 
          measuredPropertyComponent = measuredPropertyComponent, 
          frequencies = settings$Frequencies,
          buildingId = buildingId, 
          buildingSubject = buildingSubject, 
          timeColumn = "time", 
          localTimeZone = tz, 
          consumptionColumn = "real",
          indicatorsUnitsSubjects = settings$IndicatorsUnitsSubjects,
          indicatorsTimeAggregationFunctions = settings$IndicatorsTimeAggregationFunctions,
          baselineConsumptionColumn = "predicted",
          buildingGrossFloorArea = buildingGrossFloorArea, 
          eemProjectDf = eems[eems$eemProjectId==strsplit(eemProject,"\\.")[[1]][2],],
          forceAssessmentSingleEEM = settings$EEMAssessmentConditions$ForceAssessmentBySingleEEM,
          carbonEmissionsColumn = "Qe_emissionsFactor", 
          energyPriceColumn = "Qe_price", 
          modelName = modelName, 
          modelId = modelId, 
          modelLocation = modelSubject, 
          modelStorageInfrastructureSubject = "bigg:MLFlow", 
          estimateWhenAggregate = T, 
          prevResults = harmonised_results
        )
      }
    }
  }
  
  write("* Estimation finalised!",stderr())
  
  return(harmonised_results)
}