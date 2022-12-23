HourlyDynamicModel <- function(df, identifier, settings, tz, plots=T){
  
  modelName <- "HourlyDynamicModel"

  if(plots){
    dir.create(settings$OutputDataDirectory,F)
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  }
  
  # MLFlow configuration, fetch the experimentId and last registered model, if available.
  Sys.setenv(MLFLOW_PYTHON_BIN=settings$PYTHON3_BIN)
  Sys.setenv(MLFLOW_TRACKING_URI=settings$MLFlow$TrackingUri)
  Sys.setenv(MLFLOW_VERBOSE=FALSE)
  Sys.setenv(MLFLOW_BIN=settings$MLFlow$MLFLOW_BIN)
  experimentId <- tryCatch(
    mlflow_create_experiment(identifier),
    error = function(e){
      experiment <- mlflow_get_experiment(name=identifier)
      if (experiment$lifecycle_stage!="active") mlflow_restore_experiment(experiment$experiment_id)
      experiment$experiment_id
    }
  )
  if(settings$MLFlow$ForceModelTraining==F){
    lastRegisteredModel <- tryCatch(
      data.frame(
        "modelSubject"= mlflow_get_registered_model(
          paste0(identifier,"~",modelName))$latest_versions[[1]]$run_link,
        "modelId"= gsub("runs:/|/model","", mlflow_get_registered_model(
          paste0(identifier,"~",modelName))$latest_versions[[1]]$run_link),
        "modelName"= mlflow_get_registered_model(
          paste0(identifier,"~",modelName))$latest_versions[[1]]$name,
        "time"=as.POSIXct(mlflow_get_registered_model(
          paste0(identifier,"~",modelName))$latest_versions[[1]]$last_updated_timestamp/1000,
          format="%s",origin=as.POSIXct("1970-01-01 00:00:00",tz="UTC"),tz="UTC")
      ),
      error = function(e)NULL)
    if(is.null(lastRegisteredModel)){
      lastFittedModel <- NULL
    } else {
      lastFittedModel <- mlflow_load_model(lastRegisteredModel$modelSubject)
      write("",stderr())
    }
  }
  needsToBeTrained <- if(settings$MLFlow$ForceModelTraining || is.null(lastRegisteredModel)){
      TRUE
    } else {
      (now(tz="UTC") - lastRegisteredModel$time) >= 
        as.period(settings$MLFlow$ModelTrainingPeriodicity)
    }
  
  if(needsToBeTrained){
    
    ####
    # TRAIN AND PREDICT ----
    ####
    
    write("## Training process is starting",stderr())
    write(sprintf("reporter:status: TRAIN %s", identifier),stderr())
    
    if(plots){
      ts_p <- ggplot(
        reshape2::melt( df %>% select(time, Qe, temperature) %>% suppressMessages(pad()), 
                        "time")
      ) + 
        geom_line(
          aes(time,value)
        ) + 
        facet_wrap(~variable, scales = "free_y", ncol=1) +
        theme_bw()
      saveWidget(ggplotly(ts_p), 
                 paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                  "raw_consumption_temperature.html",sep="/"),
                 selfcontained = T)
    }
    
    ####
    # Calendar features and filtering of holidays and special periods ----
    ####
    
    # Detect the COVID influenced period if data is over COVID lockdown & return-to-normality period
    # with a minimum of one-year period.
    
    if( as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz) > (
          ( as.Date(min(df$time[is.finite(df$Qe)]), tz=tz) +
              months(settings$DataCleaning$CheckForDisruption$minInitialMonths)
          )
        ) ||
        ( as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz) >= 
          as.Date(settings$DataCleaning$CheckForDisruption$minIniDate)
        ) ||
        ( ( as.Date(min(df$time[is.finite(df$Qe)]), tz=tz) +
            months(settings$DataCleaning$CheckForDisruption$minInitialMonths) 
          ) <= as.Date(settings$DataCleaning$CheckForDisruption$maxEndDate) 
        )
    ) {
      
      write("* Detecting the time period with affectance by SARS-CoV2 lockdowns",stderr())
      write(sprintf("reporter:status: SARS %s", identifier),stderr())

      
      minIniDate = max(min(as.Date(df$time[is.finite(df$Qe)], tz=tz)) +
                         months(settings$DataCleaning$CheckForDisruption$minInitialMonths) ,
                       as.Date(settings$DataCleaning$CheckForDisruption$minIniDate))
      maxIniDate = max(min(as.Date(df$time[is.finite(df$Qe)], tz=tz)) +
                         months(settings$DataCleaning$CheckForDisruption$minInitialMonths) ,
                       as.Date(settings$DataCleaning$CheckForDisruption$maxIniDate))
      minEndDate = max(maxIniDate,
                       min(as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz),
                           as.Date(settings$DataCleaning$CheckForDisruption$minEndDate)))
      maxEndDate = max(minEndDate,
                       min(as.Date(quantile(df$time[is.finite(df$Qe)],0.8), tz=tz),
                           as.Date(settings$DataCleaning$CheckForDisruption$maxEndDate)))
      covid_affected_period <- detect_disruptive_period(
        data=df, consumptionColumn="Qe",
        temperatureColumn="temperature", timeColumn = "time", tz=tz,
        minIniDate = minIniDate, maxIniDate = maxIniDate,
        minEndDate = minEndDate, maxEndDate = maxEndDate, 
        checkFor=settings$DataCleaning$CheckForDisruption$checkFor,
        minDecrementPercentualAffectation = settings$DataCleaning$CheckForDisruption$minDecrementPercentualAffectation,
        minIncrementPercentualAffectation = settings$DataCleaning$CheckForDisruption$minIncrementPercentualAffectation)
      
      covid_affected_period <- tryCatch(
        seq.Date(covid_affected_period$minDate,covid_affected_period$maxDate,by = "days"),
        error = function(e)NULL)
    } else {
      covid_affected_period <- c()
    }
    
    # Detect holidays
    write("* Detecting the holidays",stderr())
    write(sprintf("reporter:status: HOLIDAYS %s", identifier),stderr())

    holidaysDates <- detect_holidays_in_tertiary_buildings(
      data = df, 
      consumptionColumn = "Qe", 
      timeColumn = "time",
      tz=tz,
      ignoreDates = covid_affected_period,
      plotDensity = F)
    
    # Add the calendar components
    df <- df %>% calendar_components(
      localTimeZone = tz,
      holidays = holidaysDates,
      inplace=T
    )
    
    if(plots){
      h <- ggplot(df[df$time>min(df$time[is.finite(df$Qe)],na.rm=T),]) + geom_line(aes(time,Qe)) +
        geom_point(data=df[df$isHolidays==T,],mapping = aes(time,Qe),col="red")
      saveWidget(ggplotly(h), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                    "holidays.html",sep="/"), 
                 selfcontained = T)
    }
    
    ####
    # Outliers detection ----
    ####
    
    write("* Detecting the outliers",stderr())
    write(sprintf("reporter:status: OUTLIERS1 %s", identifier),stderr())

    if(all(c("value","window") %in% colnames(df)))
      df <- df %>% select(-value, -window)
    df <- 
      df %>%
      select(!(contains("outliers") | contains("upperPredCalendarModel") | 
                 contains("lowerPredCalendarModel"))) %>%
      left_join(
        detect_ts_calendar_model_outliers(
          data = .,
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
          daysThatAreOutliers = covid_affected_period,
          logValueColumn = T,
          autoDetectProfiled = T),
        by = "localtime"
      )
    abnormalDays <- sort(df %>% 
      group_by(date) %>% summarise(out=sum(outliers)) %>%
      filter(out>0) %>% select(date) %>% unlist(.) %>% as.Date(.))
    df$abnormalDay <- df$date %in% abnormalDays
    
    if(plots){
      g <- ggplot(df[,c("localtime","Qe","outliers","upperPredCalendarModel","lowerPredCalendarModel")]) +
        geom_line(aes(localtime,Qe)) +
        geom_ribbon(aes(localtime,ymax=upperPredCalendarModel,ymin=lowerPredCalendarModel),
                    col="blue",alpha=0.5)
      if(!all(is.na(df$outliers) | df$outliers==F)) g <- g + geom_point(aes(localtime,ifelse(outliers,Qe,NA)),col="yellow")
      saveWidget(ggplotly(g), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                    "outliers_plot.html", sep="/"), selfcontained = T)
    }
    
    ####
    # Clustering and classification of daily load curves ----
    ####
    
    write("* Detecting the most common daily load curves",stderr())
    write(sprintf("reporter:status: CURVES %s", identifier),stderr())

    if(is.null(settings$DailyLoadCurveClustering$maxTrainingMonths)){
      maxDateForClustering <- NULL
    } else {
      ht <- hourly_timesteps(720*settings$DailyLoadCurveClustering$maxTrainingMonths,detect_time_step(df$time))
      maxDateForClustering <- sort(df$date[df$outliers==F & !is.na(df$outliers)],)[
        if(ht > length(df$date[df$outliers==F & !is.na(df$outliers)])){
          length(df$date[df$outliers==F & !is.na(df$outliers)])
        } else {ht}
      ]
    }
    clust <- clustering_dlc(
      data = df,
      consumptionFeature = "Qe", 
      outdoorTemperatureFeature = "temperature", 
      localTimeZone = tz,
      kMax = settings$DailyLoadCurveClustering$kMax, 
      kMin = settings$DailyLoadCurveClustering$kMin,
      inputVars = settings$DailyLoadCurveClustering$inputVars, 
      loadCurveTransformation = settings$DailyLoadCurveClustering$loadCurveTransformation,
      balanceOutdoorTemperatures = settings$DailyLoadCurveClustering$balanceOutdoorTemperatures,
      ignoreDates =
        df %>% group_by(date) %>% summarise(outliers=sum(outliers)>0) %>% filter(
          if(is.null(maxDateForClustering)){
            outliers==T
          } else {
            outliers==T | (date >= maxDateForClustering)
          }) %>% select(date) %>% 
        unlist %>% as.Date,
      holidaysDates = holidaysDates,
      nDayParts = settings$DailyLoadCurveClustering$nDayParts,
      normalisationMethod = "range01"
    )
    
    if("s" %in% colnames(df))
      df <- df %>% select(-s)
    df <- df %>% left_join(clust$dailyClassification, by="date")
    
    classif <- classification_dlc(
      data = df[is.na(df$s),], 
      consumptionFeature = "Qe",
      outdoorTemperatureFeature = "temperature", 
      localTimeZone = tz,
      holidaysDatesFeature = "holidaysDate",
      abnormalDaysFeature = "abnormalDay",
      clustering = clust,
      methodNormalDays = "clusteringCentroids",
      methodAbnormalDays = "classificationModel"
    )
    df <- df %>% left_join(classif$dailyClassification,by="date")
    df$s <- ifelse(!is.na(df$s.x),df$s.x,df$s.y)
    df$s_origin <- ifelse(!is.na(df$s.x),"clustering","classification")
    df$s.x <- NULL
    df$s.y <- NULL
    # df <- df[!is.na(df$s),]
    
    if(plots){
      p <- ggplot(df[df$s_origin=="clustering",] %>%
                    group_by(date) %>%
                    summarise(Qe=sum(Qe),
                              temperature=mean(temperature,na.rm=T),
                              s=first(s))) +
        geom_point(aes(temperature,Qe,col=s)) +
        xlab(bquote("temperature (ºC)")) + 
        ylab("consumption (kWh)")+ theme_bw()
      saveWidget(ggplotly(p), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                    "clustering_signature.html",sep="/"), 
                 selfcontained = T)
      p <- ggplot(df) +
          geom_line(
            aes(hour, Qe, group=date, col=s_origin),
            alpha=0.1
          ) +
          xlab("hour of the day") +
          facet_wrap(~s) +
          theme_bw()
      saveWidget(ggplotly(p), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                    "clustering_dlc.html",sep="/"), 
                 selfcontained = T)
      p_ts <- ggplot(df %>% group_by(date) %>% 
                       summarise("s"=first(s),
                                 "Qe"=sum(Qe),
                                 "s_origin"=first(s_origin))) + 
        geom_point(
          aes(date, Qe, col=s, shape=s_origin),
          alpha=0.7
        ) + 
        xlab("time") +
        theme_bw()
      saveWidget(ggplotly(p_ts), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                       "clustering_dlc_ts.html",sep="/"), 
                 selfcontained = T)
    }
    #df$s = "01"
    ###
    # Check weather dependence by group of daily load curves ----
    ###
    write("* Detecting if exists any weather dependence in energy consumption",stderr())
    write(sprintf("reporter:status: WEATHER %s", identifier),stderr())

    if(plots){
      pdf(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                "clustering_dlc_wdep.pdf",sep="/"),4,3)
    }
    weatherDependenceByCluster <- get_change_point_temperature_v2(
      consumptionData = df[ df$s_origin=="clustering",c("time","Qe","s")],#df$s==x &
      weatherData =  df[ df$s_origin=="clustering",c("time","temperature")],
      consumptionFeature = "Qe",
      temperatureFeature = "temperature",
      consumptionGroupFeature = "s",
      localTimeZone = tz,
      plot=plots
    )
    
    # weatherDependenceByCluster <- do.call(rbind,lapply(FUN = function(x){
    #   data.frame("s"=x,t(unlist(get_change_point_temperature(
    #     consumptionData = df[df$s==x & df$s_origin=="clustering",c("time","Qe")],#
    #     weatherData =  df[df$s==x & df$s_origin=="clustering",c("time","temperature")],
    #     consumptionFeature = "Qe",
    #     temperatureFeature = "temperature",
    #     localTimeZone = tz,
    #     plot=plots
    #   ))))
    # }, sort(unique(df$s))))
    if(plots){ dev.off() }
    wdep <- as.list(setNames(matrixStats::colMeans2(
      apply(as.data.frame(weatherDependenceByCluster) %>% select(-s),1:2,as.numeric),
      na.rm = T),c("tbalh","tbalc","heating","cooling")))
    df$wdeph <- factor(df$s, levels=sort(unique(df$s)))
    levels(df$wdeph) <- weatherDependenceByCluster$heating[order(levels(df$wdeph))]
    df$wdeph <- as.numeric(as.character(df$wdeph))
    df$wdepc <- factor(df$s, levels=sort(unique(df$s)))
    levels(df$wdepc) <- weatherDependenceByCluster$cooling[order(levels(df$wdepc))]
    df$wdepc <- as.numeric(as.character(df$wdepc))
    wdep$heating <- wdep$heating > 0
    wdep$cooling <- wdep$cooling > 0
    
    ###
    # Setting the model parameters and transformations to be done during the model training ----
    ###

    write("* Setting model parameters and transformations",stderr())
    write(sprintf("reporter:status: TRANSFORMATIONS %s", identifier),stderr())

    generalParams <- list(
      "nhar"=list(
        "datatype"="integer",
        "nlevels"=2,
        "min"=8,
        "max"=10
      ),
      "maxh"=list(
        "datatype"="float",
        "nlevels"=3,
        "min"=10,
        "max"=40
      ),
      "maxc"=list(
        "datatype"="float",
        "nlevels"=3,
        "min"=10,
        "max"=40
      ),
      "alpha"=list(
        "datatype"="discrete",
        "levels"=c(0.6,0.75,0.85,0.9,0.95,0.975,0.985,0.99,0.995)
      ),
      "lambda"=list(
        "datatype"="discrete",
        "levels"=c(
          get_lpf_smoothing_time_scale(data.frame("time"=df$time),
            if(wdep$heating && wdep$cooling){
             6*31*24
            } else if (wdep$heating || wdep$cooling) {
             4*31*24
            } else {
             2*31*24
            })
        )
      )
    )
    generalTransformationSentences <- list(
      # Classify the daily load curves in case it was not predicted
      "s" = "
        if(exists('s')){
          factor(
            s,
            levels=rownames(clusteringResults$absoluteLoadCurvesCentroids),
            labels=rownames(clusteringResults$absoluteLoadCurvesCentroids)
          )
        } else {
          factor(classification_dlc(
            ..., 
            consumptionFeature = 'Qe',
            outdoorTemperatureFeature = 'temperature', 
            localTimeZone = tz,
            holidaysDatesFeature = 'holidaysDate',
            abnormalDaysFeature = 'abnormalDay',
            clustering = clusteringResults,
            methodNormalDays = 'clusteringCentroids',
            methodAbnormalDays = 'classificationModel'
          )$sRaw,
          levels=rownames(clusteringResults$absoluteLoadCurvesCentroids),
          labels=rownames(clusteringResults$absoluteLoadCurvesCentroids))
        }",
      
      "intercept_s" = c("s"),
      
      # Avoid errors when there is no holidays detected
      "isHolidays" = "
        if(length(unique(as.character(isHolidays)))==1){
          rep(1,isHolidays)
        } else {
          isHolidays
        }",
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "daily_seasonality" = c(
        "fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)"
        ,"weekday"
      ),
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "weekly_seasonality" = c(
        "fs_components(...,featuresName='weekdayNum',nHarmonics=2,inplace=F)"#,
        #"s"
      ),
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "yearly_seasonality" = c(
        "fs_components(...,featuresName='monthInt',nHarmonics=2,inplace=F)"#,
        #"s"
      ),
      
      # Fill some gaps in the outdoor temperature time series.
      "temperature" = "na.locf(
                          na.locf(
                            na.approx(temperature,na.rm = F),
                            fromLast = T,na.rm = T
                          ),
                          na.rm=T)",
      
      # Low Pass Filtered (LPF) outdoor temperature
      "tlpf" = "lpf_ts(...,featuresNames='temperature',smoothingTimeScaleParameter=param$alpha,
                       inplace=F)",
      
      # Depending the cluster and the weather dependence analysis, define activations of the HVAC system
      "onOffHeating" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = weatherDependenceByCluster$heating)
                        ))",
      "onOffCooling" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = weatherDependenceByCluster$cooling)
                        ))",
      "temperatureBalanceHeating" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = ifelse(is.finite(weatherDependenceByCluster$tbalh),
                                                 weatherDependenceByCluster$tbalh,18))
                          ))",
      "temperatureBalanceCooling" = "as.numeric(as.character(
                          factor(s,levels=weatherDependenceByCluster$s,
                                 labels = ifelse(is.finite(weatherDependenceByCluster$tbalc),
                                                 weatherDependenceByCluster$tbalc,24))
                          ))",
      
      # Estimate the heating degrees based on a heating balance temperature 
      # and the LPF temperature series
      "heatingLpf" = "degree_raw(...,featuresName='tlpf',
                        baseTemperature=NULL,
                        baseTemperatureName='temperatureBalanceHeating',
                        mode='heating',outputFeaturesName='heatingLpf',maxValue=param$maxh,
                        hysteresisBaseTemperature = 0,
                        inplace=F)",
      
      # Estimate the cooling degrees based on a cooling balance temperature 
      # and the LPF temperature series
      "coolingLpf" = "degree_raw(...,featuresName='tlpf',
                        baseTemperature=NULL,
                        baseTemperatureName='temperatureBalanceCooling',
                        mode='cooling',outputFeaturesName='coolingLpf',maxValue=param$maxc,
                        hysteresisBaseTemperature = 0,
                        inplace=F)",
      
      # Squared versions of the heating and cooling degrees
      "heatingLpf2" = "heatingLpf^2",
      "coolingLpf2" = "coolingLpf^2",
      
      # Check if exists heating or cooling degrees at every timestep 
      "heatingLpfBool" = "ifelse(heatingLpf>0,1,0)",
      "coolingLpfBool" = "ifelse(coolingLpf>0,1,0)",
      
      # Regression components for the heating degrees depending on the hour of the day 
      #and weekday/weekend
      "th"=c("heatingLpf",
             "fs_components(...,featuresName='hour',
                            nHarmonics=ceiling(param$nhar/2),inplace=F)",
             "s"),
      
      # Regression components for the cooling degrees depending on the hour of the day 
      # and weekday/weekend
      "tc"=c("coolingLpf",
             "fs_components(...,featuresName='hour',
                            nHarmonics=ceiling(param$nhar/2),inplace=F)",
             
             "s"),
      
      # Regression components for the heating degrees depending on the hour of the day 
      #and weekday/weekend
      "th2"=c("heatingLpf2",
              "fs_components(...,featuresName='hour',
                             nHarmonics=ceiling(param$nhar/2),inplace=F)",
              "s"),
      
      # Regression components for the cooling degrees depending on the hour of the day 
      # and weekday/weekend
      "tc2"=c("coolingLpf2",
              "fs_components(...,featuresName='hour',
                             nHarmonics=ceiling(param$nhar/2),inplace=F)",
              "s"
      ),
      
      # Regression components for the heating intercept depending on the hour 
      # of the day and weekday/weekend
      "thint"=c("heatingLpfBool",
                "s"
      ),
      
      # Regression components for the cooling intercept depending on the hour 
      # of the day and weekday/weekend
      "tcint"=c("coolingLpfBool",
                "s"
      )
    )
    
    trControl <- trainControl(method="none")
    
    ###
    # Model training ----
    ###
    
    write("* Training of the model and loading process to MLFlow",stderr())
    write(sprintf("reporter:status: MLFLOW %s", identifier),stderr())

    with(mlflow_start_run(experiment_id = experimentId), {
      
      if(wdep$heating && wdep$cooling){
        write("   Model type: Heating and cooling",stderr())
        params <- generalParams[c("nhar","maxh","maxc","alpha","lambda")]#,"tbalc","tbalh"
        transformationSentences <- generalTransformationSentences[
          c("s","intercept_s","daily_seasonality","weekly_seasonality","yearly_seasonality",
            "temperature","tlpf","temperatureBalanceHeating","temperatureBalanceCooling",
            "onOffHeating","onOffCooling","heatingLpf","coolingLpf",
            "heatingLpf2","coolingLpf2","heatingLpfBool","coolingLpfBool",
            "th","tc","th2","tc2","tcint","thint")]
        minMonthsTraining <- 9
      	HC_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ daily_seasonality + tc + th + tcint + thint,
            data=df,
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
            logOutput = T,
            minMonthsTraining = minMonthsTraining,
            continuousTime = T,
            transformationSentences = args$transformationSentences,
            weatherDependenceByCluster = args$weatherDependenceByCluster,
            clusteringResults = args$clusteringResults
          )
        }
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- HC_model(X, df, ...)
            expected <- df$Qe
            obtained <- biggr::predict.train(
              object = mod, 
              newdata = df, 
              forceGlobalInputFeatures = NULL,
              modelMinMaxHorizonInHours = 1,
              modelWindow = NULL,
              modelSelection = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 3,
          popSize = 8,
          df = df[df$outliers==F & !is.na(df$outliers),],
          parallel = F,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
        mod <- HC_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
      } else if (wdep$heating){
        write("   Model type: Only heating",stderr())
        params <- generalParams[c("nhar","maxh","alpha","lambda")]#,"tbalh"
        transformationSentences <- generalTransformationSentences[
          c("s","intercept_s","daily_seasonality","weekly_seasonality","yearly_seasonality",
            "temperature","tlpf","temperatureBalanceHeating","onOffHeating",
            "heatingLpf","heatingLpf2",
            "heatingLpfBool","th","th2","thint")]
        minMonthsTraining <- 4
        H_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ daily_seasonality + th + thint,
            data = df,
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
            logOutput = T,
            minMonthsTraining = minMonthsTraining,
            continuousTime = T,
            transformationSentences = args$transformationSentences,
            weatherDependenceByCluster = args$weatherDependenceByCluster,
            clusteringResults = args$clusteringResults
          )
        }
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- H_model(X, df, ...)
            expected <- df$Qe
            obtained <- biggr::predict.train(
              object = mod, 
              newdata = df, 
              forceGlobalInputFeatures = NULL,
              modelMinMaxHorizonInHours = 1,
              modelWindow = NULL,
              modelSelection = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 3,
          popSize = 8,
          parallel = F,
          df = df[df$outliers==F & !is.na(df$outliers),],
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
        mod <- H_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
      } else if (wdep$cooling){
        write("   Model type: Only Cooling",stderr())
        params <- generalParams[c("nhar","maxc","alpha","lambda")]#,"tbalc"
        transformationSentences <- generalTransformationSentences[
          c("s","intercept_s","daily_seasonality","weekly_seasonality","yearly_seasonality",
            "temperature","tlpf","temperatureBalanceCooling","onOffCooling",
            "coolingLpf","coolingLpf2",
            "coolingLpfBool","tc","tc2","tcint")]
        minMonthsTraining <- 4
        C_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ daily_seasonality + tc + tcint,
            data = df,
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            maxPredictionValue = max(df$Qe,na.rm=T) * 1.1,
            logOutput = T,
            minMonthsTraining = minMonthsTraining,
            continuousTime = T,
            transformationSentences = args$transformationSentences,
            weatherDependenceByCluster = args$weatherDependenceByCluster,
            clusteringResults = args$clusteringResults
          )
        }
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- C_model(X, df, ...)
            expected <- df$Qe
            obtained <- biggr::predict.train(
              object = mod, 
              newdata = df, 
              forceGlobalInputFeatures = NULL,
              modelMinMaxHorizonInHours = 1,
              modelWindow = NULL,
              modelSelection = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 3,
          popSize = 8,
          parallel = F,
          df = df[df$outliers==F & !is.na(df$outliers),],
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
        mod <- C_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
      } else {
        write("   Model type: Not weather dependence",stderr())
        params <- generalParams[c("nhar","lambda")]
        transformationSentences <- generalTransformationSentences[
          c("s","intercept_s","daily_seasonality","weekly_seasonality","yearly_seasonality")
        ]
        minMonthsTraining <- 1
        CAL_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ intercept_s + daily_seasonality,
            data=df,
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            maxPredictionValue = max(df[df$outliers==F & !is.na(df$outliers),"Qe"],na.rm=T) * 1.1,
            logOutput = T,
            minMonthsTraining = minMonthsTraining,
            continuousTime = T,
            transformationSentences = args$transformationSentences,
            clusteringResults = args$clusteringResults
          )
        }
        best_params <- hyperparameters_tuning(
          opt_criteria = "minimise",
          opt_function = function(X, df, ...) {
            mod <- CAL_model(X, df, ...)
            expected <- df$Qe
            obtained <- biggr::predict.train(
              object = mod, 
              newdata = df, 
              forceGlobalInputFeatures = NULL,
              modelMinMaxHorizonInHours = 1,
              modelWindow = NULL,
              modelSelection = NULL
            )
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 3,
          popSize = 8,
          parallel = F,
          df = df[df$outliers==F & !is.na(df$outliers),],
          transformationSentences = transformationSentences,
          clusteringResults = clust
        )
        mod <- CAL_model(best_params, df[df$outliers==F & !is.na(df$outliers),],
          transformationSentences = transformationSentences,
          clusteringResults = clust
        )
      }
      
      # Generate the predictor object
      predictor <- crate(function(x, forceGlobalInputFeatures = NULL,modelMinMaxHorizonInHours=1,
                                  modelWindow="%Y-%m-%d", modelSelection="rmse"){
        biggr::predict.train(
          object = !!mod, 
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          modelMinMaxHorizonInHours = modelMinMaxHorizonInHours,
          modelWindow = modelWindow,
          modelSelection = modelSelection
        )
      })

      df_result <- df[df$time >= (min(df$time)+months(minMonthsTraining)),]
      df_result$predicted <- predictor(df_result)
      
      if(plots){
        # Actual vs. predicted
        p <- ggplotly(
          ggplot(df_result) + 
            geom_line(aes(time,Qe), col="black") + 
            geom_line(aes(time,predicted),col="red",alpha=0.5))
        saveWidget(p, paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                            "series_comparison.html",sep="/"), 
                   selfcontained = T)
        
        # Residuals plot
        p <- ggplotly(
          ggplot(df_result) + 
            geom_line(aes(time,Qe-predicted), col="black"))
        saveWidget(p, paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                            "residuals.html",sep="/"), 
                   selfcontained = T)
      }
      
      # Generate the MLflow instance
      mlflow_log_param("Model",modelName)
      
      # Store the params
      for (i in 1:ncol(mod$bestTune)){
        mlflow_log_param(colnames(mod$bestTune)[i],as.character(mod$bestTune[1,i]))
      }
      # Store the errors
      mlflow_log_metric("MAE", 
                        MAE(df_result$Qe[df_result$outliers==F],
                            df_result$predicted[df_result$outliers==F],na.rm = T))
      mlflow_log_metric("RMSE", RMSE(df_result$Qe[df_result$outliers==F],
                                     df_result$predicted[df_result$outliers==F],na.rm = T))
      mlflow_log_metric("CVRMSE", 
                        RMSE(df_result$Qe[df_result$outliers==F],
                             df_result$predicted[df_result$outliers==F],na.rm = T)/
                          mean(df_result$Qe[df_result$outliers==F],na.rm=T))
      mlflow_log_metric("r2", cor(df_result$Qe[df_result$outliers==F],
                                  df_result$predicted[df_result$outliers==F],
                                  use = "na.or.complete")^2)
      # Store the artifacts
      if(plots){
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "raw_consumption_temperature.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "series_comparison.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "residuals.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "outliers_plot.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "clustering_signature.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "clustering_dlc.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "clustering_dlc_ts.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "clustering_dlc_wdep.pdf",sep="/"))
      }
      
      # Register the model
      tryCatch(mlflow_create_registered_model(paste0(identifier,"~",modelName)),error=function(e){})
      modelId <- mlflow_get_run()$run_uuid[1]
      modelSubject <- sprintf("runs:/%s/model",modelId)
      registeredModelName <- paste0(identifier,"~",modelName)
      mlflow_create_model_version(name = registeredModelName,
                                  run_link = modelSubject, source="bigg")
      
      # Store the model
      mlflow_log_model(predictor,"model")
      mlflow_log_param("modelId",modelId)
      mlflow_log_param("modelSubject",modelSubject)
      mlflow_log_param("registeredModelName",registeredModelName)
      
    })
    
  } else if (needsToBeTrained == F){
    
    ####
    # PREDICT ----
    ####
    
    write("## Reusing the last trained model",stderr())
    write(sprintf("reporter:status: REUSING %s", identifier),stderr())

    
    predictor <- lastFittedModel
    modelSubject <- lastRegisteredModel$modelSubject
    modelId <- lastRegisteredModel$modelId
    
    ####
    # Calendar features and filtering of holidays and special periods ----
    ####
    
    # Detect the COVID influenced period if data is over COVID lockdown & return-to-normality period
    # with a minimum of one-year period.
    
    if( ( as.Date(max(df$time[is.finite(df$Qe)]), tz=tz) >= 
          as.Date(settings$DataCleaning$CheckForDisruption$minIniDate) ) ||
        ( as.Date(min(df$time[is.finite(df$Qe)]), tz=tz) <= 
          as.Date(settings$DataCleaning$CheckForDisruption$maxEndDate) )
    ) {
      
      write("* Detecting the time period with affectance by SARS-CoV2 lockdowns",stderr())
      write(sprintf("reporter:status: SARS %s", identifier),stderr())

      
      minIniDate = max(min(as.Date(df$time[is.finite(df$Qe)], tz=tz)),
                       as.Date(settings$DataCleaning$CheckForDisruption$minIniDate))
      maxIniDate = max(min(as.Date(df$time[is.finite(df$Qe)], tz=tz)),
                       as.Date(settings$DataCleaning$CheckForDisruption$maxIniDate))
      minEndDate = max(maxIniDate,
                       min(max(as.Date(df$time[is.finite(df$Qe)], tz=tz)),
                           as.Date(settings$DataCleaning$CheckForDisruption$minEndDate)))
      maxEndDate = max(minEndDate,
                       min(max(as.Date(df$time[is.finite(df$Qe)], tz=tz)),
                           as.Date(settings$DataCleaning$CheckForDisruption$maxEndDate)))
      covid_affected_period <- detect_disruptive_period(
        data=df, consumptionColumn="Qe",
        temperatureColumn="temperature", timeColumn = "time", tz=tz,
        minIniDate = minIniDate, maxIniDate = maxIniDate,
        minEndDate = minEndDate, maxEndDate = maxEndDate)
      
      tryCatch(
        seq.Date(covid_affected_period$minDate,covid_affected_period$maxDate,by = "days"),
        error = function(e)NULL)
    } else {
      covid_affected_period <- c()
    }
    
    # Detect holidays
    write("* Detecting the holidays",stderr())
    write(sprintf("reporter:status: HOLIDAYS %s", identifier),stderr())

    holidaysDates <- detect_holidays_in_tertiary_buildings(
      data = df, 
      consumptionColumn = "Qe", 
      timeColumn = "time",
      tz=tz,
      ignoreDates = covid_affected_period,
      plotDensity = F)
    
    # Add the calendar components
    df <- df %>% calendar_components(
      localTimeZone = tz,
      holidays = holidaysDates,
      inplace=T
    )
    
    ####
    # Outliers detection ----
    ####
    
    write("* Detecting the outliers",stderr())
    write(sprintf("reporter:status: OUTLIERS %s", identifier),stderr())

    if(all(c("value","window") %in% colnames(df)))
      df <- df %>% select(-value, -window)
    df <- 
      df %>%
      select(!(contains("outliers") | contains("upperPredCalendarModel") | 
                 contains("lowerPredCalendarModel"))) %>%
      left_join(
        detect_ts_calendar_model_outliers(
          data = .,
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
          daysThatAreOutliers = covid_affected_period,
          logValueColumn = T,
          autoDetectProfiled = F),
        by = "localtime"
      )
    abnormalDays <- df %>% 
      group_by(date) %>% summarise(out=sum(outliers)) %>%
      filter(out>0) %>% select(date) %>% unlist(.) %>% as.Date(.)
    df$abnormalDay <- df$date %in% abnormalDays
    
    ###
    # Define the dataset for the indicators calculation ----
    ###
    df_result <- df
  }
  
  ####
  # CONSUMPTION PREDICTIONS - Total, baseload, heating and cooling ----
  ####
  
  write("* Estimating the consumption based on the trained model",stderr())
  write(sprintf("reporter:status: ESTIMATING %s", identifier),stderr())

  # Total energy consumption prediction
  total <- predictor(df_result)
  
  # Forcing only heating and only cooling dependency
  baseload_and_cooling <- predictor( df_result, 
                                     forceGlobalInputFeatures = list("heatingLpf2"=0, "heatingLpf"=0, "heatingLpfBool"=0) )
  baseload_and_heating <- predictor( df_result,
                                     forceGlobalInputFeatures = list("coolingLpf2"=0, "coolingLpf"=0, "coolingLpfBool"=0) )
  
  # Estimate the baseload consumption along the period
  baseload <- predictor( df_result,
                         forceGlobalInputFeatures = list("heatingLpf2"=0,"heatingLpf"=0, "heatingLpfBool"=0, 
                                                         "coolingLpf2"=0,"coolingLpf"=0, "coolingLpfBool"=0) )
  
  # plot(baseload_and_cooling,type="l",col="blue")
  # lines(baseload_and_heating,col="red")
  # lines(baseload,col="black")
  
  # Disaggregated predicted components and actual consumption
  disaggregated_df <- data.frame(
    "time"=df_result$time,
    "temperature"=df_result$temperature,
    "real"=df_result$Qe,
    "predicted"=total,
    "baseload"=baseload,
    "heating"=baseload_and_heating-baseload, 
    "cooling"=baseload_and_cooling-baseload
  )
  disaggregated_df$heatingSmooth <- rollmean(ifelse(disaggregated_df$heating>0,
                                                    disaggregated_df$heating,0), k = 3,
                                             align = "center", partial = T, fill = c(0,0,0))
  disaggregated_df$coolingSmooth <- rollmean(ifelse(disaggregated_df$cooling>0,
                                                    disaggregated_df$cooling,0), k = 3,
                                             align = "center", partial = T, fill = c(0,0,0))
  disaggregated_df$real_ <- ifelse(is.finite(disaggregated_df$real),disaggregated_df$real,
                                   disaggregated_df$predicted)
  disaggregated_df$baseloadR <- ifelse(disaggregated_df$baseload > disaggregated_df$real_,
                                       disaggregated_df$real_, disaggregated_df$baseload)
  disaggregated_df$heatingR <- ifelse(
    disaggregated_df$heatingSmooth > 0.1,
    ifelse(
      disaggregated_df$heatingSmooth > 0.1 & disaggregated_df$coolingSmooth > 0.1,
      # Heating and cooling at the same time
      (disaggregated_df$real_ - disaggregated_df$baseloadR) *
        (disaggregated_df$heatingSmooth/
           (disaggregated_df$heatingSmooth+disaggregated_df$coolingSmooth)),
      # Only heating
      disaggregated_df$real_ - disaggregated_df$baseloadR),
    0
  )
  disaggregated_df$coolingR <- ifelse(
    disaggregated_df$coolingSmooth > 0.1,
    disaggregated_df$real_ - (disaggregated_df$baseloadR + disaggregated_df$heatingR),
    0)
  disaggregated_df$baseloadR <- disaggregated_df$real_ - (disaggregated_df$coolingR + disaggregated_df$heatingR)
  
  disaggregated_df$real_ <- NULL
  
  # plot(disaggregated_df$baseloadR+disaggregated_df$coolingR,type="l",col="blue")
  # lines(disaggregated_df$baseloadR+disaggregated_df$heatingR,col="red")
  # lines(disaggregated_df$baseloadR,col="black")
  
  df_result <- df_result[,!(colnames(df_result) %in% colnames(disaggregated_df)[colnames(disaggregated_df)!="time"])] %>% 
    left_join(disaggregated_df,by="time")
  
  write("Success!",stderr())
  write(sprintf("reporter:status: SUCCESS %s", identifier),stderr())
  write("",stderr())
  
  return(
    list(
      data = df_result,
      subject = modelSubject,
      id = modelId,
      name = modelName
    )
  )
}
