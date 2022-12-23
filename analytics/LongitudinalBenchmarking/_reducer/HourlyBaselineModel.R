HourlyBaselineModel <- function(df, identifier, settings, plots=T){
  
  modelName <- "HourlyBaselineModel"
  
  if(plots)
    dir.create(paste(settings$OutputDataDirectory,"plots",sep="/"),F)
  
  # MLFlow configuration, fetch the experimentId and last registered model, if available.
  Sys.setenv(MLFLOW_PYTHON_BIN=paste0(settings$MLFlow$WorkingDirectory,"/bin/python3"))
  Sys.setenv(MLFLOW_TRACKING_URI=settings$MLFlow$TrackingUri)
  Sys.setenv(MLFLOW_VERBOSE=FALSE)
  Sys.setenv(MLFLOW_BIN=paste0(settings$MLFlow$WorkingDirectory,"/bin/mlflow"))
  experimentId <- tryCatch(
    mlflow_create_experiment(identifier, artifact_location = paste0(mlflow_wd,"/mlruns")),
    error = function(e){
      experiment <- mlflow_get_experiment(name=identifier)
      if (experiment$lifecycle_stage!="active") mlflow_restore_experiment(experiment$experiment_id)
      experiment$experiment_id
    }
  )
  lastRegisteredModel <- tryCatch(
    data.frame(
      "modelUri"= mlflow_get_registered_model(
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
    lastFittedModel <- mlflow_load_model(lastRegisteredModel$modelUri)
    write("",stdout())
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
    
    write("## Training process is starting",stdout())
    write("",stdout())
    
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
    
    if( ( as.Date(max(df$time[is.finite(df$Qe)]), tz=tz) >= 
          as.Date(settings$DataCleaning$CheckForDisruption$minIniDate) ) ||
        ( as.Date(min(df$time[is.finite(df$Qe)]), tz=tz) <= 
          as.Date(settings$DataCleaning$CheckForDisruption$maxEndDate) )
    ) {
      
      write("* Detecting the time period with affectance by SARS-CoV2 lockdowns",stdout())
      
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
      
      covid_affected_period <- 
        seq.Date(covid_affected_period$minDate,covid_affected_period$maxDate,by = "days")
    } else {
      covid_affected_period <- c()
    }
    
    # Detect holidays
    write("* Detecting the holidays",stdout())
    holidaysDates <- detect_holidays_in_tertiary_buildings(
      data = df, 
      consumptionColumn = "Qe", 
      timeColumn = "time",
      tz=tz,
      ignoreDates = covid_affected_period)
    
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
    
    write("* Detecting the outliers",stdout())
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
    
    if(plots){
      g <- ggplot(df[,c("localtime","Qe","outliers","upperPredCalendarModel","lowerPredCalendarModel")]) +
        geom_line(aes(localtime,Qe)) +
        geom_ribbon(aes(localtime,ymax=upperPredCalendarModel,ymin=lowerPredCalendarModel),
                    col="blue",alpha=0.5)
      if(!all(df$outliers==F)) g <- g + geom_point(aes(localtime,ifelse(outliers,Qe,NA)),col="yellow")
      saveWidget(ggplotly(g), paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                    "outliers_plot.html", sep="/"), selfcontained = T)
    }
    
    ####
    # Clustering and classification of daily load curves ----
    ####
    
    write("* Detecting the most common daily load curves",stdout())
    clust <- clustering_dlc(
      data = df,
      consumptionFeature = "Qe", 
      outdoorTemperatureFeature = "temperature", 
      localTimeZone = tz,
      kMax = settings$DailyLoadCurveClustering$kMax, 
      kMin = settings$DailyLoadCurveClustering$kMin,
      inputVars = settings$DailyLoadCurveClustering$inputVars, 
      loadCurveTransformation = settings$DailyLoadCurveClustering$loadCurveTransformation,
      ignoreDates =
        df %>% group_by(date) %>% summarise(outliers=sum(outliers)>0) %>% filter(
          if(is.null(settings$DailyLoadCurveClustering$maxTrainingMonths)){
            outliers==T
          } else {
            outliers==T | (date >= (min(date)+months(settings$DailyLoadCurveClustering$maxTrainingMonths)))
          }) %>% select(date) %>% 
        unlist %>% as.Date,
      holidaysDates = holidaysDates,
      nDayParts = settings$DailyLoadCurveClustering$nDayParts
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
    df <- df[!is.na(df$s),]
    
    if(plots){
      p <- ggplot(df %>% filter(outliers==F)) + 
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
    
    ###
    # Check weather dependence by group of daily load curves ----
    ###
    write("* Detecting if exists any weather dependence in energy consumption",stdout())
    if(plots){
      pdf(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),"clustering_dlc_wdep.pdf",sep="/"),4,3)
    }
    weatherDependenceByCluster <- do.call(rbind,lapply(FUN = function(x){
      data.frame("s"=x,t(unlist(get_change_point_temperature(
        consumptionData = df[df$s==x & df$outliers==F,c("time","Qe")],
        weatherData =  df[df$s==x & df$outliers==F,c("time","temperature")],
        consumptionFeature = "Qe",
        temperatureFeature = "temperature",
        localTimeZone = tz,
        plot=plots
      ))))
    }, sort(unique(df$s))))
    if(plots){ dev.off() }
    wdep = as.list(setNames(matrixStats::colMeans2(
      apply(as.data.frame(weatherDependenceByCluster) %>% select(-s),1:2,as.numeric),
      na.rm = T),c("tbal","heating","cooling")))
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
    
    write("* Setting model parameters and transformations",stdout())
    generalParams <- list(
      "nhar"=list(
        "datatype"="integer",
        "nlevels"=0,
        "min"=10,
        "max"=10
      ),
      "maxh"=list(
        "datatype"="float",
        "nlevels"=0,
        "min"=50,
        "max"=50
      ),
      "maxc"=list(
        "datatype"="float",
        "nlevels"=0,
        "min"=50,
        "max"=50
      ),
      "alpha"=list(
        "datatype"="float",
        "nlevels"=0,
        "min"=0.9,
        "max"=0.9
      ),
      "lambda"=list(
        "datatype"="discrete",
        "levels"=c(
          get_lpf_smoothing_time_scale(data.frame("time"=df$time),
                                       if(wdep$heating && wdep$cooling){
                                         9*31*24
                                       } else if (wdep$heating || wdep$cooling) {
                                         4*31*24
                                       } else {
                                         1*31*24
                                       })
        )
      )
    )
    generalTransformationSentences <- list(
      # Generate a fake dayType variable
      "dayType" = "vectorial_transformation(
                   as.factor(
                    ifelse(isHolidays==T,'holidays',
                     as.character(weekday))
                   ), outputFeatureName = 'dayType'
                 )",
      
      # Classify the daily load curves in case it was not predicted
      "s" = "vectorial_transformation(
        if(exists('s')){
          s
        } else {
          classification_dlc(
            ..., 
            consumptionFeature = 'Qe',
            outdoorTemperatureFeature = 'temperature', 
            localTimeZone = tz,
            holidaysDatesFeature = 'holidaysDate',
            abnormalDaysFeature = 'abnormalDay',
            clustering = clusteringResults,
            methodNormalDays = 'clusteringCentroids',
            methodAbnormalDays = 'classificationModel'
          )$sRaw
        }, 
        outputFeatureName = 's'
      )",
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "weekhour" = c(
        "fs_components(...,featuresName='hour',nHarmonics=param$nhar,inplace=F)",
        "s"
      ),
      
      # Fourier series components of the hour of the day by weekdays and weekends. 
      "yearday" = c(
        "fs_components(...,featuresName='dayYear',nHarmonics=4,inplace=F)",
        "s"
      ),
      
      # Fill some gaps in the outdoor temperature time series.
      "temperature" = "vectorial_transformation(
                        na.locf(
                          na.locf(
                            na.approx(temperature,na.rm = F),
                            fromLast = T,na.rm = T
                          ),
                          na.rm=T),
                      outputFeatureName='temperature')",
      
      # Low Pass Filtered (LPF) outdoor temperature
      "tlpf" = "lpf_ts(...,featuresNames='temperature',smoothingTimeScaleParameter=param$alpha,
                outputFeaturesNames='temperatureLpf')",
      
      # Depending the cluster and the weather dependence analysis, define activations of the HVAC system
      "onOffHeating" = "vectorial_transformation(
                              as.numeric(
                                as.character(
                                  factor(s,levels=weatherDependenceByCluster$s,labels = weatherDependenceByCluster$heating)
                              )),
                              outputFeatureName='onOffHeating')",
      "onOffCooling" = "vectorial_transformation(
                              as.numeric(
                                as.character(
                                  factor(s,levels=weatherDependenceByCluster$s,labels = weatherDependenceByCluster$cooling)
                              )),
                              outputFeatureName='onOffCooling')",
      "temperatureBalance" = "vectorial_transformation(
                                as.numeric(
                                  as.character(
                                    factor(s,levels=weatherDependenceByCluster$s,labels = ifelse(
                                      is.finite(weatherDependenceByCluster$tbal),weatherDependenceByCluster$tbal,18))
                                  )
                                ),
                                outputFeatureName='temperatureBalance')",
      
      # Estimate the heating degrees based on a heating balance temperature 
      # and the LPF temperature series
      "heatingLpf" = "degree_raw(...,featuresName='temperatureLpf',
                      baseTemperature=temperatureBalance,
                      mode='heating',outputFeaturesName='heatingLpf',maxValue=param$maxh,
                      inplace=F)",
      
      # Estimate the cooling degrees based on a cooling balance temperature 
      # and the LPF temperature series
      "coolingLpf" = "degree_raw(...,featuresName='temperatureLpf',
                      baseTemperature=temperatureBalance,
                      mode='cooling',outputFeaturesName='coolingLpf',maxValue=param$maxc,
                      inplace=F)",
      
      # Squared versions of the heating and cooling degrees
      "heatingLpf2" = "vectorial_transformation(heatingLpf^2,outputFeatureName='heatingLpf2')",
      "coolingLpf2" = "vectorial_transformation(coolingLpf^2,outputFeatureName='coolingLpf2')",
      
      # Check if exists heating or cooling degrees at every timestep 
      "heatingLpfBool" = "vectorial_transformation(ifelse(heatingLpf>0,1,0),
                          outputFeatureName='heatingLpfBool')",
      "coolingLpfBool" = "vectorial_transformation(ifelse(coolingLpf>0,1,0),
                          outputFeatureName='coolingLpfBool')",
      
      # Regression components for the heating degrees depending on the hour of the day 
      #and weekday/weekend
      "th"=c("heatingLpf",
             "fs_components(...,featuresName='hour',nHarmonics=ceiling(param$nhar/2),inplace=F)",
             "onOffHeating",
             "s"
      ),
      
      # Regression components for the cooling degrees depending on the hour of the day 
      # and weekday/weekend
      "tc"=c("coolingLpf",
             "fs_components(...,featuresName='hour',nHarmonics=ceiling(param$nhar/2),inplace=F)",
             "onOffCooling",
             "s"
      ),
      
      # Regression components for the heating degrees depending on the hour of the day 
      #and weekday/weekend
      "th2"=c("heatingLpf2",
              "fs_components(...,featuresName='hour',nHarmonics=ceiling(param$nhar/2),inplace=F)",
              "onOffHeating",
              "s"
      ),
      
      # Regression components for the cooling degrees depending on the hour of the day 
      # and weekday/weekend
      "tc2"=c("coolingLpf2",
              "fs_components(...,featuresName='hour',nHarmonics=ceiling(param$nhar/2),inplace=F)",
              "onOffCooling",
              "s"
      ),
      
      # Regression components for the heating intercept depending on the hour 
      # of the day and weekday/weekend
      "thint"=c("heatingLpfBool",
                "fs_components(...,featuresName='hour',nHarmonics=ceiling(param$nhar/2),inplace=F)",
                "onOffHeating",
                "s"
      ),
      
      # Regression components for the cooling intercept depending on the hour 
      # of the day and weekday/weekend
      "tcint"=c("coolingLpfBool",
                "fs_components(...,featuresName='hour',nHarmonics=ceiling(param$nhar/2),inplace=F)",
                "onOffCooling",
                "s"
      )
    )
    
    trControl <- trainControl(method="none")
    
    ###
    # Model training ----
    ###
    
    write("* Training of the model and loading process to MLFlow",stdout())
    with(mlflow_start_run(experiment_id = experimentId), {
      parent.env(environment())

      # Generate the predictor object
      predictor <- crate(function(x, mod, forceGlobalInputFeatures = NULL,modelMinMaxHorizonInHours=1,
                                  modelWindow="%Y-%m-%d", modelSelection="rmse"){
        biggr::predict.train(
          object = mod, 
          newdata = x, 
          forceGlobalInputFeatures = forceGlobalInputFeatures,
          modelMinMaxHorizonInHours = modelMinMaxHorizonInHours,
          modelWindow = modelWindow,
          modelSelection = modelSelection
        )
      })


      if(wdep$heating && wdep$cooling){
        write("   Model type: Heating and cooling",stdout())
        params <- generalParams[c("nhar","maxh","maxc","alpha","lambda")]
        transformationSentences <- generalTransformationSentences[
          c("dayType","s","yearday","weekhour","temperature",
            "tlpf","temperatureBalance",
            "onOffHeating","onOffCooling","heatingLpf","coolingLpf",
            "heatingLpf2","coolingLpf2","heatingLpfBool","coolingLpfBool",
            "th","tc","th2","tc2","tcint","thint")]
        # transformationSentences <<- transformationSentences
	HC_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ weekhour + th + tc,
            data=df[df$outliers==F & is.finite(df$Qe),],
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            # maxPredictionValue = max(df[df$outliers==F,"Qe"],na.rm=T) * 1.05,
            logOutput = T,
            minMonthsTraining = 9,
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
            obtained <- predictor(df, mod)
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 1,
          df = df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
        mod <- HC_model(best_params, df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
      } else if (wdep$heating){
        write("   Model type: Only heating",stdout())
        params <- generalParams[c("nhar","maxh","alpha","lambda")]
        transformationSentences <- generalTransformationSentences[
          c("dayType","s","yearday","weekhour","temperature","tlpf",
            "temperatureBalance","onOffHeating",
            "heatingLpf","heatingLpf2",
            "heatingLpfBool","th","th2","thint")]
        # transformationSentences <<- transformationSentences
        H_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ weekhour + th,
            data=df[df$outliers==F & is.finite(df$Qe),],
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            # maxPredictionValue = max(df[df$outliers==F,"Qe"],na.rm=T) * 1.05,
            logOutput = T,
            minMonthsTraining = 4,
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
            obtained <- predictor(df, mod)
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 1,
          df = df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
        mod <- H_model(best_params, df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
      } else if (wdep$cooling){
        write("   Model type: Only Cooling",stdout())
        params <- generalParams[c("nhar","maxc","alpha","lambda")]
        transformationSentences <- generalTransformationSentences[
          c("dayType","s","yearday","weekhour","temperature","tlpf",
            "temperatureBalance","onOffCooling",
            "coolingLpf","coolingLpf2",
            "coolingLpfBool","tc","tc2","tcint")]
        # transformationSentences <<- transformationSentences
        C_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ weekhour + tc,
            data=df[df$outliers==F & is.finite(df$Qe),],
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            # maxPredictionValue = max(df[df$outliers==F,"Qe"],na.rm=T) * 1.05,
            logOutput = T,
            minMonthsTraining = 4,
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
            obtained <- predictor(df, mod)
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 1,
          df = df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
        mod <- C_model(best_params, df,
          transformationSentences = transformationSentences,
          weatherDependenceByCluster = weatherDependenceByCluster,
          clusteringResults = clust
        )
      } else {
        write("   Model type: Not weather dependence",stdout())
        params <- generalParams[c("nhar","lambda")]
        transformationSentences <- generalTransformationSentences[
          c("dayType","s","yearday","weekhour")
        ]
        # transformationSentences <<- transformationSentences
        CAL_model <- function(params, df, ...) {
          args <- list(...)
          train(
            Qe ~ weekhour,
            data=df[df$outliers==F & is.finite(df$Qe),],
            method = RLS(
              data.frame(parameter = names(params),
                         class = mapply(names(params),FUN=function(i) generalParams[[i]][["datatype"]]))
            ),
            tuneGrid = expand.grid(params), 
            trControl = trControl,
            # maxPredictionValue = max(df[df$outliers==F,"Qe"],na.rm=T) * 1.05,
            logOutput = T,
            minMonthsTraining = 1,
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
            obtained <- predictor(df, mod)
            RMSE(expected, obtained, na.rm = T)
          },
          features = params,
          maxiter = 1,
          df = df,
          transformationSentences = transformationSentences,
          clusteringResults = clust
        )
        mod <- CAL_model(best_params, df,
          transformationSentences = transformationSentences,
          clusteringResults = clust
        )
      }
      
      df_result <- df
      df_result$predicted <- predictor(df_result, mod)
      
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
                                  "clustering_dlc.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "clustering_dlc_ts.html",sep="/"))
        mlflow_log_artifact(paste(paste(settings$OutputDataDirectory,"plots",sep="/"),
                                  "clustering_dlc_wdep.pdf",sep="/"))
      }
      
      # Register the model
      tryCatch(mlflow_create_registered_model(paste0(identifier,"~",modelName)),error=function(e){})
      modelId <- mlflow_get_run()$run_uuid[1]
      modelUri <- sprintf("runs:/%s/model",modelId)
      registeredModelName <- paste0(identifier,"~",modelName)
      mlflow_create_model_version(name = registeredModelName,
                                  run_link = modelUri)
      
      # Store the model
      mlflow_log_model(predictor,"model")
      mlflow_log_param("modelId",modelId)
      mlflow_log_param("modelUri",modelUri)
      mlflow_log_param("registeredModelName",registeredModelName)
      
    })
    
  } else if (needsToBeTrained == F){
    
    ####
    # PREDICT ----
    ####
    
    write("## Reusing the last trained model",stdout())
    write("",stdout())
    
    predictor <- lastFittedModel
    modelUri <- lastRegisteredModel$modelUri
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
      
      write("* Detecting the time period with affectance by SARS-CoV2 lockdowns",stdout())
      
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
      
      covid_affected_period <- 
        seq.Date(covid_affected_period$minDate,covid_affected_period$maxDate,by = "days")
    } else {
      covid_affected_period <- c()
    }
    
    # Detect holidays
    write("* Detecting the holidays",stdout())
    holidaysDates <- detect_holidays_in_tertiary_buildings(
      data = df, 
      consumptionColumn = "Qe", 
      timeColumn = "time",
      tz=tz,
      ignoreDates = covid_affected_period)
    
    # Add the calendar components
    df <- df %>% calendar_components(
      localTimeZone = tz,
      holidays = holidaysDates,
      inplace=T
    )
    
    ####
    # Outliers detection ----
    ####
    
    write("* Detecting the outliers",stdout())
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
  
  ###
  # Total, baseload, heating and cooling prediction
  ###
  
  write("* Estimating the consumption based on the trained model",stdout())  
  # Total energy consumption prediction
  total <- predictor(df_result, mod)
  
  # Forcing only heating and only cooling dependency
  baseload_and_cooling <- predictor( df_result, mod, 
                                     forceGlobalInputFeatures = list("heatingLpf2"=0, "heatingLpf"=0, "heatingLpfBool"=0) )
  baseload_and_heating <- predictor( df_result, mod,
                                     forceGlobalInputFeatures = list("coolingLpf2"=0, "coolingLpf"=0, "coolingLpfBool"=0) )
  
  # Estimate the baseload consumption along the period
  baseload <- predictor( df_result, mod, 
                         forceGlobalInputFeatures = list("heatingLpf2"=0,"heatingLpf"=0, "heatingLpfBool"=0, 
                                                         "coolingLpf2"=0,"coolingLpf"=0, "coolingLpfBool"=0) )
  
  #plot(baseload_and_cooling,type="l",col="blue");lines(baseload_and_heating,col="red");lines(baseload,col="black")
  
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
  disaggregated_df$baseloadR <- ifelse(disaggregated_df$baseload > disaggregated_df$real,
                                       disaggregated_df$real, disaggregated_df$baseload)
  disaggregated_df$heatingR <- ifelse(
    disaggregated_df$heatingSmooth > 0.1,
    ifelse(
      disaggregated_df$heatingSmooth > 0.1 & disaggregated_df$coolingSmooth > 0.1,
      # Heating and cooling at the same time
      (disaggregated_df$real - disaggregated_df$baseloadR) *
        (disaggregated_df$heatingSmooth/
           (disaggregated_df$heatingSmooth+disaggregated_df$coolingSmooth)),
      # Only heating
      disaggregated_df$real - disaggregated_df$baseloadR),
    0
  )
  disaggregated_df$coolingR <- ifelse(
    disaggregated_df$coolingSmooth > 0.1,
    disaggregated_df$real - (disaggregated_df$baseloadR + disaggregated_df$heatingR),
    0)
  disaggregated_df$baseloadR <- disaggregated_df$real - (disaggregated_df$coolingR + disaggregated_df$heatingR)
  
  df_result <- df_result %>% left_join(disaggregated_df,by="time")
  
  write(sprintf("* End of building %s", identifier),stdout())
  return(
    list(
      data = df_result,
      uri = modelUri,
      id = modelId,
      name = modelName
    )
  )
}
