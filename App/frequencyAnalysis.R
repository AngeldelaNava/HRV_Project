frequencyAnalysis <- function(input, output, session, hrv.data){
  a = c(input$ULF1, input$ULF2, input$VLF1,
        input$VLF2, input$LF1, input$LF2,
        input$HF1, input$HF2)
  
  hrv.data = CalculatePSD(hrv.data, indexFreqAnalysis = 1, doPlot = FALSE)
  
  output$freq_analysis <- renderPlot({
    PlotPSD(hrv.data, indexFreqAnalysis = 1, ULFmin = a[1], ULFmax = a[2],
            VLFmin = a[3], VLFmax = a[4], LFmin = a[5], LFmax = a[6],
            HFmin = a[7], HFmax = a[8])
  })
  
  hrv.data = CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, size = input$size_tf,
                                shift = input$shift, type = "fourier",
                                ULFmin = input$ULF1_tf, ULFmax = input$ULF2_tf,
                                VLFmin = input$VLF1_tf, VLFmax = input$VLF2_tf,
                                LFmin = input$LF1_tf, LFmax = input$LF2_tf,
                                HFmin = input$HF1_tf, HFmax = input$HF2_tf)
  
  output$power_band <- renderPlot({
    PlotPowerBand(hrv.data, indexFreqAnalysis = 1)
  })
  
  BANDS = c("ULF", "VLF", "LF", "HF")
  ENERGY = CalculateEnergyInPSDBands(hrv.data, indexFreqAnalysis = 1,
                                     ULFmax = (input$ULF2),
                                     VLFmin = (input$VLF1),
                                     VLFmax = (input$VLF2),
                                     LFmin = (input$LF1),
                                     LFmax = (input$LF2),
                                     HFmin = (input$HF1),
                                     HFmax = (input$HF2))
  
  output$freq_table <- renderTable({
    df <- t(data.frame(ENERGY))
    colnames(df) <- BANDS
    df
  })
  return (hrv.data)
}