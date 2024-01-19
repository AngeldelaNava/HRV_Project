tabsControl <- function(input, output, session){
  tab <- input$t2 == "Frequency Analysis"
  tab2 <- input$t2 == "Time Analysis"
  tab3 <- input$t2 == "Time-Freq Analysis"
  if (tab) {
    updateTabsetPanel(session, inputId = "t1",
                      selected = "Frequency Analysis")
  } else if (tab2) {
    updateTabsetPanel(session, inputId = "t1", selected = "Time Analysis")
  } else if (tab3) {
    updateTabsetPanel(session, inputId = "t1", selected = "Time-Freq Analysis")
  } else {
    tab <- input$t3 == "Embedding Dimension & Timelag"
    tab2 <- input$t3 == "Correlation Dimension"
    if (tab){
      updateTabsetPanel(session, inputId = "t1",
                        selected = "Embedding Dimension & Timelag")
    } else if (tab2) {
      updateTabsetPanel(session, inputId = "t1",
                        selected = "Correlation Dimension")
    } else {
      updateTabsetPanel(session, inputId = "t1",
                        selected = "Maximum Lyapunov")
    }
  }
}