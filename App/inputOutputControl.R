source("timeAnalysis.R")
source("frequencyAnalysis.R")
source("nonlinearAnalysis.R")
source("csv.R")

inputOutputControl <- function(input, output, session, hrv.data){
    
    req(input$upload)
    
    tryCatch({
      hrv.data = LoadBeatAscii(hrv.data, input$upload$datapath)
      hrv.data = BuildNIHR(hrv.data)
      if(input$filter_button){
        hrv.data = FilterNIHR(hrv.data)
      }
      A <- c()
      B <- c()
      if(input$size > 0 & input$interval > 0.0){
        hrv.data = CreateTimeAnalysis(hrv.data, size = input$size,
                                      interval = input$interval)
        timeAnalysis(input, output, session, hrv.data)
        
        CHARACTERISTICS = c("Size", "Interval", "SDNN (msec.)", "SDANN (msec.)",
                            "SDNNIDX (msec.)", "pNN50 (%)", "SDSD (msec.)",
                            "r-MSSD (msec.)", "IRRR (msec.)", "MADRR (msec.)",
                            "TINN (msec.)", "HRV index")
        VALUES = c(input$size, input$interval, hrv.data$TimeAnalysis[[1]]$SDNN,
                   hrv.data$TimeAnalysis[[1]]$SDANN,
                   hrv.data$TimeAnalysis[[1]]$SDNNIDX,
                   hrv.data$TimeAnalysis[[1]]$pNN50,
                   hrv.data$TimeAnalysis[[1]]$SDSD,
                   hrv.data$TimeAnalysis[[1]]$rMSSD,
                   hrv.data$TimeAnalysis[[1]]$IRRR,
                   hrv.data$TimeAnalysis[[1]]$MADRR,
                   hrv.data$TimeAnalysis[[1]]$TINN,
                   hrv.data$TimeAnalysis[[1]]$HRVi)
        output$csv_button <- downloadHandler(
          filename = function(){
            paste("time_analysis.csv")
          },
          content = function(file){
            csvDownload(cbind(CHARACTERISTICS, VALUES), file)
          }
        )
        A <- csvSum(CHARACTERISTICS, c())
        B <- csvSum(VALUES, c())
      }
      
      #if(input$csv_button){
      #  csvDownload(cbind(CHARACTERISTICS, VALUES), "time_analysis.csv")
      #}
      
      tryCatch({
        if(input$freqhr > 0){
          hrv.data = InterpolateNIHR(hrv.data, freqhr = input$freqhr)
          hrv.data = CreateFreqAnalysis(hrv.data)
          
          hrv.data <- frequencyAnalysis(input, output, session, hrv.data)
          
          BANDS = c("ULF", "VLF", "LF", "HF")
          ENERGY = CalculateEnergyInPSDBands(hrv.data, indexFreqAnalysis = 1,
                                             ULFmax = (input$ULF2),
                                             VLFmin = (input$VLF1),
                                             VLFmax = (input$VLF2),
                                             LFmin = (input$LF1),
                                             LFmax = (input$LF2),
                                             HFmin = (input$HF1),
                                             HFmax = (input$HF2))
          #if(input$csv_button_f){
          #  csvDownload(cbind(BANDS, ENERGY), "freq_analysis.csv")
          #}
          output$csv_button_f <- downloadHandler(
            filename = function(){
              paste("freq_analysis.csv")
            },
            content = function(file){
              csvDownload(cbind(BANDS, ENERGY), file)
            }
          )
          A <- csvSum(A, BANDS)
          B <- csvSum(B, ENERGY)
        }
        

        #hide(input$eDSpinner)
        #hide(input$tLSpinner)
        #hide(input$cDSpinner)
        #hide(input$mLSpinner)
        if(input$start_nla){
          tryCatch({
            if(input$lagMax > -1 & input$maxEmbeddingDim > 0){
              hrv.data = CreateNonLinearAnalysis(hrv.data)
              kTimeLag = CalculateTimeLag(
                hrv.data, method = "first.minimum", lagMax = input$lagMax,
                doPlot = FALSE)
              timeLag(input, output, session, hrv.data, kTimeLag)
              #hide(input$tLSpinner)
              kEmbeddingDim = CalculateEmbeddingDim(
                hrv.data, numberPoints = input$numberPoints, timeLag = kTimeLag,
                maxEmbeddingDim = input$maxEmbeddingDim, doPlot = FALSE)
              embeddingDim(input, output, session, hrv.data, kTimeLag,
                           kEmbeddingDim)
              output$starnl_loading_text <- renderText({
                "Non-linear analysis started"
              })
              #hide(input$eDSpinner)
              if(input$start_cd){
                tryCatch({
                  
                  if(input$minRadius < input$maxRadius & input$minRadius > 0 & input$pointsRadius > 0 & input$theilerWindow > 0){
                    hrv.data = CalculateCorrDim(
                      hrv.data, indexNonLinearAnalysis = 1,
                      minEmbeddingDim = kEmbeddingDim - 1,
                      maxEmbeddingDim = kEmbeddingDim + 2, timeLag = kTimeLag,
                      minRadius = input$minRadius, maxRadius = input$maxRadius,
                      pointsRadius = input$pointsRadius,
                      theilerWindow = input$theilerWindow, doPlot = FALSE)
                    correlationDimensionCalculation(output, hrv.data)
                    output$starcd_loading_text <- renderText({
                      "Correlation dimension calculated"
                    })
                    #hide(input$cDSpinner)
                    if(input$reg_correlation & input$minRegC > -1 & input$minRegC < input$maxRegC){
                      
                      #hide(output$corr_plot)
                      #show(input$cDSpinner)
                      hrv.data = EstimateCorrDim(
                        hrv.data, indexNonLinearAnalysis = 1,
                        regressionRange = c(input$minRegC, input$maxRegC),
                        useEmbeddings = (kEmbeddingDim - 1):(kEmbeddingDim + 2),
                        doPlot = FALSE)
                      A <- csvSum(A, "Correlation Statistic")
                      C <- hrv.data$NonLinearAnalysis[[1]]$correlation$statistic
                      B <- csvSum(B, C)
                      correlationDimensionEstimation(input, output, kEmbeddingDim,
                                                     hrv.data)
                      output$cd_estim_loading_text <- renderText({
                        "Estimation finished"
                      })
                      #show(output$corr_plot)
                      #hide(input$cDSpinner)
                      correlatonStatistic(output, hrv.data)
                      C = hrv.data$NonLinearAnalysis[[1]]$correlation$statistic
                      #if(input$csv_button_c){
                      #  result <- cbind(Statistic = "Correlation Statistic", Value = C)
                      #  csvDownload(result, "corr_analysis.csv")
                      #}
                      output$csv_button_c <- downloadHandler(
                        filename = function(){
                          paste("corr_analysis.csv")
                        },
                        content = function(file){
                          result <- cbind(Statistic = "Correlation Statistic", Value = C)
                          csvDownload(result, file)
                        }
                      )
                    }
                  }
                  
                  #show(input$cDSpinner)
                  
                },
                error = function(e) {
                  cat("Error: Correlation dimension analysis failed")
                })
              }
              
              if(input$start_lya & input$radius > 0 & input$theilerWindowLya > 0){
                tryCatch({
                  
                  #show(input$mLSpinner)
                  hrv.data = CalculateMaxLyapunov(
                    hrv.data, indexNonLinearAnalysis = 1,
                    minEmbeddingDim = kEmbeddingDim,
                    maxEmbeddingDim = kEmbeddingDim + 2, timeLag = kTimeLag,
                    radius = input$radius, theilerWindow = input$theilerWindowLya,
                    doPlot = FALSE)
                  lyapunovCalculation(output, hrv.data)
                  output$starML_loading_text <- renderText({
                    "Maximum Lyapunov exponent calculated"
                  })
                  #hide(input$mLSpinner)
                  if(input$reg_lya & input$minRegL > -1 & input$minRegL < input$maxRegL){
                    
                    #hide(output$lya_plot)
                    #show(input$mLSpinner)
                    hrv.data = EstimateMaxLyapunov(
                      hrv.data, indexNonLinearAnalysis = 1,
                      regressionRange = c(input$minRegL, input$maxRegL),
                      useEmbeddings = (kEmbeddingDim):(kEmbeddingDim + 2),
                      doPlot = FALSE)
                    A <- csvSum(A, "Max. Lyapunov Statistic")
                    C = hrv.data$NonLinearAnalysis[[1]]$lyapunov$statistic
                    B <- csvSum(B, C)
                    lyapunovEstimation(input, output, kEmbeddingDim, hrv.data)
                    #show(output$lya_plot)
                    #hide(input$mLSpinner)
                    lyapunovStatistic(output, hrv.data)
                    output$ML_estimation_loading_text <- renderText({
                      "Estimation finished"
                    })
                    #if(input$csv_button_ml){
                    #  result <- cbind(Statistic = "Max. Lyapunov Statistic", Value = C)
                    #  csvDownload(result, "lya_analysis.csv")
                    #}
                    output$csv_button_ml <- downloadHandler(
                      filename = function(){
                        paste("lya_analysis.csv")
                      },
                      content = function(file){
                        result <- cbind(Statistic = "Max. Lyapunov Statistic", Value = C)
                        csvDownload(result, file)
                      }
                    )
                  }
                }, error = function(e) {
                  cat("Error: Maximum Lyapunov exponent calculation failed")
                })
              }
            }
            #show(input$eDSpinner)
            #show(input$tLSpinner)
            
            
          },
          error = function(e) {
            cat("Error: non-linear analysis failed\n")
            stop(safeError(e))
          })
        }
      },
      error = function(e) {
        cat("Error: data filtering failed\n")
        stop(safeError(e))
      })
      
      #if(input$downloadButton){
      #  result <- cbind(Variables = A, Values = B)
      #  csvDownload(result, "analysis.csv")
      #}
      output$downloadButton <- downloadHandler(
        filename = function(){
          paste("analysis.csv")
        },
        content = function(file){
          result <- cbind(Variables = A, Values = B)
          csvDownload(result, file)
        }
      )
    },
    error = function(e) {
      cat("Error: upload failed\n")
      stop(safeError(e))
    })
}