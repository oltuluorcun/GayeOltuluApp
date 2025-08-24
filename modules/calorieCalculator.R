# ----------------------------
# Calorie Calculator Module
# ----------------------------

calorieCalculatorUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(title = "Girdi Alanları", width = 6, solidHeader = TRUE, status = "primary", height = 500,
          numericInput(ns("Sut"), "Porsiyon - Süt", 0),
          numericInput(ns("Et"), "Porsiyon - Et", 0),
          numericInput(ns("Ekmek"), "Porsiyon - Ekmek", 0),
          numericInput(ns("Sebze"), "Porsiyon - Sebze", 0),
          numericInput(ns("Meyve"), "Porsiyon - Meyve", 0),
          numericInput(ns("Yag"), "Porsiyon - Yağ", 0)
      ),
      box(title = "Çıktı Alanları", width = 6, solidHeader = TRUE, status = "primary", height = 500,
          dataTableOutput(ns("CalorieTable"))
      )
    ),
    br(),
    fluidRow(
      infoBoxOutput(ns("infoKarbonhidrat"), width = 3),
      infoBoxOutput(ns("infoProtein"), width = 3),
      infoBoxOutput(ns("infoYag"), width = 3),
      infoBoxOutput(ns("infoTotal"), width = 3)
    )
  )
}

calorieCalculatorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    CalorieData <- reactive({ 
      Porsiyon <- c(input$Sut, input$Et, input$Ekmek, input$Sebze, input$Meyve, input$Yag)
      data.frame(
        Karbonhidrat = Porsiyon * c(9,0,15,7,12,0),
        Protein = Porsiyon * c(6,6,2,2,0,0),
        Yag = Porsiyon * c(6,5,0,0,0,5)
      )
    })
    
    output$CalorieTable <- renderDataTable({
      CalorieData() %>% 
        dplyr::mutate(Rownames = c("Süt","Et","Ekmek","Sebze","Meyve","Yağ")) %>% 
        tibble::column_to_rownames("Rownames")
    })
    
    output$infoKarbonhidrat <- renderInfoBox({
      infoBox("Karbonhidrat", paste("Kalori:", 4 * sum(CalorieData()$Karbonhidrat),"Kalori"),
              paste("Yüzde:", round(100 * 4 * sum(CalorieData()$Karbonhidrat) / sum(colSums(CalorieData())* c(4,4,9)),3),"%"))
    })
    
    output$infoProtein <- renderInfoBox({
      infoBox("Protein", paste("Kalori:", 4 * sum(CalorieData()$Protein),"Kalori"),
              paste("Yüzde:", round(100 * 4 * sum(CalorieData()$Protein) / sum(colSums(CalorieData())* c(4,4,9)),3),"%"))
    })
    
    output$infoYag <- renderInfoBox({
      infoBox("Yağ", paste("Kalori:", 9 * sum(CalorieData()$Yag),"Kalori"),
              paste("Yüzde:", round(100 * 9 * sum(CalorieData()$Yag) / sum(colSums(CalorieData()) * c(4,4,9)),3),"%"))
    })
    
    output$infoTotal <- renderInfoBox({
      infoBox("Toplam Kalori", paste("Toplam:", sum(colSums(CalorieData()) * c(4,4,9)), "Kalori"))
    })
  })
}
