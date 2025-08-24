library(shiny)
library(shinyalert)
library(DT)
library(shinyjs)

# Yeni Diyet UI
newDietUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    actionButton(ns("ogun_ekle"), "Öğün Ekle"),
    br(), br(),
    DTOutput(ns("diet_table"))
  )
}

# Yeni Diyet Server
newDietServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      data = data.frame(
        Saat = character(),
        Öğün = character(),
        Açıklama = character(),
        YaklasikKalori = numeric(),
        stringsAsFactors = FALSE
      )
    )
    
    # Öğün ekle butonu
    observeEvent(input$ogun_ekle, {
      shinyalert(
        title = "Öğün Ekle",
        html = TRUE,
        text = tagList(
          textInput(ns("meal_time"), "Saat"),
          textInput(ns("meal_name"), "Öğün Adı"),
          textAreaInput(ns("meal_desc"), "Açıklama"),
          numericInput(ns("meal_kcal"), "Yaklaşık Kalori", value = 0)
        ),
        showCancelButton = TRUE,
        confirmButtonText = "Ekle"
      )
    })
    
    # Shinyalert confirm
    observeEvent(input$meal_name, {
      req(input$meal_name)
      rv$data <- rbind(rv$data, data.frame(
        Saat = input$meal_time,
        Öğün = input$meal_name,
        Açıklama = input$meal_desc,
        YaklasikKalori = input$meal_kcal,
        stringsAsFactors = FALSE
      ))
      
      # Temizle
      updateTextInput(session, "meal_time", value = "")
      updateTextInput(session, "meal_name", value = "")
      updateTextAreaInput(session, "meal_desc", value = "")
      updateNumericInput(session, "meal_kcal", value = 0)
    })
    
    # DT sadece veri varsa
    output$diet_table <- renderDT({
      req(nrow(rv$data) > 0)
      datatable(
        rv$data,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = 't',
          columnDefs = list(list(targets = 0, className = "dt-center"))
        )
      )
    })
  })
}
