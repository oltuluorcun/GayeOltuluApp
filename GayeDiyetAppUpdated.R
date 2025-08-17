# prep ------
library(shiny)
library(shinydashboard)
library(DT)
library(openxlsx)
library(shinyjs)
library(shinyauthr)
library(NHANES)
library(tibble)
library(highcharter)
library(htmlwidgets)
library(dplyr)
library(magrittr)

# sample logins dataframe with passwords hashed by sodium package
user_base <- tibble(
  user = c("admin", "user2"),
  password = sapply(c("admin", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("admin", "User Two")
)

# ui part -----
ui <- dashboardPage(
  dashboardHeader(title = "Uzm. Dyt. Gaye Oltulu",
                  tags$li(class = "dropdown", style = "padding: 8px;", shinyauthr::logoutUI("logout"))
  ),
  dashboardSidebar(
    collapsed = TRUE, sidebarMenuOutput("sidebar")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyauthr::loginUI("login"),
    tags$head(
      tags$style(
        HTML(" 
      body { font-family: Arial, sans-serif; background-color: #f8f9fa; } 
      .container-fluid { max-width: 90%; } 
      .form-control { border-radius: 8px; } 
      .shiny-input-container { margin-bottom: 15px; } 
      .btn { font-size: 14px; margin: 5px; } 
      .dataTables_wrapper { margin-top: 10px; } 
      td { white-space: pre-wrap !important; }
          "))
    ),
    tabItems(
      tabItem("NewDiet", uiOutput("NewDiet_ui")),
      tabItem("PreviousDiets", uiOutput("PreviousDiets_ui")),
      tabItem("CalorieCalculator", uiOutput("CalorieCalculator_ui")),
      tabItem("BMICalculator", uiOutput("BMICalculator_ui"))
    )
  )
)

# server part ----
server <- function(input, output, session) {
  
  # Login -----
  logout_init <- reactiveVal(FALSE)
  
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  logout_init(callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth)))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  # Sidebar ----
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(
      id = "tabs",
      menuItem("Yeni Diyet", tabName = "NewDiet", icon = icon("person")),
      menuItem("Geçmiş Diyetler", tabName = "PreviousDiets", icon = icon("database")),
      menuItem("Kalori Hesaplama", tabName = "CalorieCalculator", icon = icon("calculator")), 
      menuItem("BMI Hesaplama", tabName = "BMICalculator", icon = icon("weight-scale", lib = "font-awesome"))
    )
  })
  
  # page 1: New Diet ----
  output$NewDiet_ui <- renderUI({
    req(credentials()$user_auth)
    fluidRow(
      box(
        title = "Input Fields", 
        width = 6, solidHeader = TRUE, status = "primary",
        fluidRow(
          column(4, actionButton("add_input", "➕ Add Input", class = "btn btn-primary")),
          column(4, actionButton("remove_input", "➖ Remove Input", class = "btn btn-warning"))
        ),
        uiOutput("newDiet_inputdiv")
      ),
      box(
        title = "Data Table", 
        width = 6, solidHeader = TRUE, status = "info",
        column(width = 6, downloadButton("download_data", "Download Excel", class = "btn btn-success")),
        column(width = 6, actionButton("saveDatabase", "Veri Tabanına Kaydet", icon = icon("save"), class = "btn btn-success")),
        DTOutput("data_table"),
        textAreaInput("Notlar", label = "Notlar", rows = 3)
      )
    )
  })
  
  max_rows <- 20
  num_inputs <- reactiveVal(6)
  
  observe({
    current <- num_inputs()
    if (current < max_rows) {
      for (i in (current + 1):max_rows) {
        shinyjs::hide(paste0("row_", i))
      }
    }
  })
  
  observeEvent(input$add_input, {
    current <- num_inputs()
    if (current < max_rows) {
      current <- current + 1
      num_inputs(current)
    }
  })
  
  observeEvent(input$remove_input, {
    current <- num_inputs()
    if (current > 1) {
      shinyjs::hide(paste0("row_", current))
      current <- current - 1
      num_inputs(current)
    }
  })
  
  getInputValue <- function(id) {
    val <- input[[id]]
    if (is.null(val)) "" else val
  }
  
  # reactive values for table
  values <- reactive({
    n <- num_inputs()
    time_vals <- sapply(1:n, function(i) {
      val <- input[[paste0("time_", i)]]
      if (is.null(val)) "" else val
    })
    text_vals <- sapply(1:n, function(i) {
      val <- input[[paste0("text_", i)]]
      if (is.null(val)) "" else val
    })
    data.frame(Time = time_vals, Text = text_vals, stringsAsFactors = FALSE)
  })
  
  # only rebuild UI when num_inputs() changes
  observeEvent(num_inputs(), {
    n <- num_inputs()
    
    # capture current input values once using isolate
    time_vals <- sapply(1:n, function(i) isolate(input[[paste0("time_", i)]]))
    text_vals <- sapply(1:n, function(i) isolate(input[[paste0("text_", i)]]))
    
    output$newDiet_inputdiv <- renderUI({
      n <- num_inputs()
      
      # capture current input values safely
      time_vals <- sapply(1:n, function(i) {
        val <- isolate(input[[paste0("time_", i)]])
        if (is.null(val)) "" else val
      })
      
      text_vals <- sapply(1:n, function(i) {
        val <- isolate(input[[paste0("text_", i)]])
        if (is.null(val)) "" else val
      })
      
      div(
        id = "fixed_inputs",
        lapply(1:n, function(i) {
          fluidRow(
            id = paste0("row_", i),
            class = "input-row",
            column(4,
                   textInput(paste0("time_", i), paste("Time", i), value = time_vals[i])
            ),
            column(4,
                   textAreaInput(paste0("text_", i), paste("Text", i), rows = 3, value = text_vals[i])
            )
          )
        })
      )
    })
    
  })
  
  
  output$data_table <- renderDT({
    df <- values()
    df$Delete <- sprintf(
      '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_row\', %d, {priority: \'event\'})">X</button>',
      seq_len(nrow(df))
    )
    datatable(df, escape = FALSE, rownames = FALSE, options = list(autoWidth = TRUE, pageLength = num_inputs()))
  }, server = FALSE)
  
  observeEvent(input$delete_row, {
    i <- input$delete_row
    updateTextInput(session, paste0("time_", i), value = "")
    updateTextAreaInput(session, paste0("text_", i), value = "")
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste("table_data-", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      df <- values()
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      writeData(wb, "Data", df)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # page 2: Previous Diets ----
  output$PreviousDiets_ui <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(h1("user input gelecek")),
      fluidRow(
        column(width = 6, h1("previous data")),
        column(width = 6, h1("notlar gelecek"))
      )
    )
  })
  
  # page 3: Calorie Calculator ----
  output$CalorieCalculator_ui <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        box(title = "Input Fields", width = 6, solidHeader = TRUE, status = "primary", height = 500,
            numericInput("Sut","Porsiyon - Sut",0),
            numericInput("Et","Porsiyon - Et",0),
            numericInput("Ekmek","Porsiyon - Ekmek",0),
            numericInput("Sebze","Porsiyon - Sebze",0),
            numericInput("Meyve","Porsiyon - Meyve",0),
            numericInput("Yag","Porsiyon - Yağ",0)
        ),
        box(title = "Output Fields", width = 6, solidHeader = TRUE, status = "primary", height = 500,
            dataTableOutput("CalorieTable"))
      ),
      br(),
      fluidRow(
        infoBoxOutput(width = 3, "infoKarbonhidrat"),
        infoBoxOutput(width = 3, "infoProtein"),
        infoBoxOutput(width = 3, "infoYag"),
        infoBoxOutput(width = 3, "infoTotal")
      )
    )
  })
  
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
      mutate(Rownames = c("Sut","Et","Ekmek","Sebze","Meyve","Yag")) %>% 
      column_to_rownames("Rownames")
  })
  
  output$infoKarbonhidrat <- renderInfoBox({
    infoBox("Karbonhidrat", paste("Kalori:", 4 * sum(CalorieData()$Karbonhidrat),"Kalori"),
            paste("Yuzde:", round(100 * 4 * sum(CalorieData()$Karbonhidrat) / sum(colSums(CalorieData())* c(4,4,9)),3),"%"))
  })
  output$infoProtein <- renderInfoBox({
    infoBox("Protein", paste("Kalori:", 4 * sum(CalorieData()$Protein),"Kalori"),
            paste("Yuzde:", round(100 * 4 * sum(CalorieData()$Protein) / sum(colSums(CalorieData())* c(4,4,9)),3),"%"))
  })
  output$infoYag <- renderInfoBox({
    infoBox("Yag", paste("Kalori:", 9 * sum(CalorieData()$Yag),"Kalori"),
            paste("Yuzde:", round(100 * 9 * sum(CalorieData()$Yag) / sum(colSums(CalorieData()) * c(4,4,9)),3),"%"))
  })
  output$infoTotal <- renderInfoBox({
    infoBox("Total Kalori", paste("Total:", sum(colSums(CalorieData()) * c(4,4,9)), "Kalori"))
  })
  
  # page 4: BMI Calculator ----
  output$BMICalculator_ui <- renderUI({
    req(credentials()$user_auth)
    fluidPage(
      fluidRow(
        box(title = "Input Fields", width = 6, solidHeader = TRUE, status = "primary", height = 300,
            numericInput("Height","Boy - Santimetre",180),
            numericInput("Weight","Ağırlık - Kilogram",70)
        ),
        box(title = "Output Fields", width = 6, solidHeader = TRUE, status = "primary", height = 300,
            br(), p(h3("BMI Hesaplama")), br(), br(), textOutput("bmi_out"))
      ),
      fluidRow(highchartOutput("BMI_Chart"))
    )
  })
  
  bmi <- reactive({ round(input$Weight / (input$Height^2) * 10000, 3) })
  
  output$bmi_out <- renderText({ paste("BMI:", bmi()) })
  
  output$BMI_Chart <- renderHighchart({
    df <- NHANES[!is.na(NHANES$BMI), ]
    d <- density(df$BMI)
    bmiValue <- bmi()
    bmiValue_density <- as.numeric(approx(d$x, d$y, xout = bmiValue)$y)
    highchart() %>%
      hc_xAxis(title = list(text = "BMI")) %>%
      hc_yAxis(title = list(text = "BMI Yoğunluk")) %>%
      hc_add_series(data = list_parse2(data.frame(x = d$x, y = d$y)), type = "spline",
                    name = "BMI Density", color = "#2b908f", marker = list(enabled = FALSE)) %>%
      hc_add_series(data = list(c(bmiValue, bmiValue_density)), type = "scatter",
                    name = "Value", color = "red", marker = list(symbol = "cross", radius = 8))
  })
}

# Run the app ----
runApp(shinyApp(ui = ui, server = server), port = 443, host = "0.0.0.0", launch.browser = TRUE)
