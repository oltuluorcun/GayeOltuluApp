# prep ------

library(shiny)
library(shinydashboard)
library(DT)
library(openxlsx)
library(shinyjs)
library(shinyauthr)
library(NHANES)

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
  # setup a sidebar menu to be rendered server-side
  dashboardSidebar(
    collapsed = TRUE, sidebarMenuOutput("sidebar")
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    # put the shinyauthr login ui module here
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
    
    # setup any tab pages you want after login here with uiOutputs
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
  
  # login status and info will be managed by shinyauthr module and stores here
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  # logout status managed by shinyauthr module and stored here
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  # this opens or closes the sidebar on login/logout
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  # Menu Items ----
  
  # only when credentials()$user_auth is TRUE, render your desired sidebar menu
  output$sidebar <- renderMenu({
    req(credentials()$user_auth)
    sidebarMenu(
      id = "tabs",
      menuItem("Yeni Diyet", tabName = "NewDiet", icon = icon("person")),
      menuItem("Geçmiş Diyetler", tabName = "PreviousDiets", icon = icon("database")),
      menuItem("Kalori Hesaplama", tabName = "CalorieCalculator", icon = icon("calculator")), 
      menuItem("BMI Hesaplama", tabName = "BMICalculator", icon = icon("weight-scale", lib = "font-awesome"))
      # Additional menu items can be added here later.
    )
  })
  
  
  # page 1 -----
  
  output$NewDiet_ui <- renderUI({
    req(credentials()$user_auth)
    
    fluidRow(
      # Left box: Input Fields
      box(
        title = "Input Fields", 
        width = 6, 
        solidHeader = TRUE, 
        status = "primary",
        
        fluidRow(
          column(4, actionButton("add_input", "➕ Add Input", class = "btn btn-primary")),
          column(4, actionButton("remove_input", "➖ Remove Input", class = "btn btn-warning"))
        ),
        uiOutput("newDiet_inputdiv"),
        
      ),
      # Right box: Data Table and Download Button
      box(
        title = "Data Table", 
        width = 6, 
        solidHeader = TRUE, 
        status = "info",
        column(
          width = 6,
          downloadButton("download_data", "Download Excel", class = "btn btn-success"),
        ),
        column(width = 6,
               actionButton("saveDatabase", "Veri Tabanına Kaydet", icon = icon("save"), class = "btn btn-success")
               ),
        DTOutput("data_table"),
        textAreaInput("Notlar", label = "Notlar", rows = 3)
      )
    )
  })
  
  # Maximum number of input rows (fixed in the UI)
  max_rows <- 20
  # Start with 6 visible rows
  num_inputs <- reactiveVal(6)
  
  # Initially hide rows beyond the current visible count.
  observe({
    current <- num_inputs()
    if (current < max_rows) {
      for (i in (current + 1):max_rows) {
        hide(paste0("row_", i))
      }
    }
  })
  
  # When "Add Input" is clicked, show the next hidden row.
  observeEvent(input$add_input, {
    current <- num_inputs()
    if (current < max_rows) {
      current <- current + 1
      num_inputs(current)
      show(paste0("row_", current))
    }
  })
  
  # When "Remove Input" is clicked, hide the last visible row.
  observeEvent(input$remove_input, {
    current <- num_inputs()
    if (current > 1) {
      hide(paste0("row_", current))
      current <- current - 1
      num_inputs(current)
    }
  })
  
  # Build a reactive data frame from the visible inputs.
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
  
  output$newDiet_inputdiv <- renderUI({
    
    div(
      id = "fixed_inputs",
      lapply(1:num_inputs(), function(i) {
        fluidRow(
          id = paste0("row_", i),
          class = "input-row",
          column(4,
                 textInput(paste0("time_", i), label = paste("Time", i))
          ),
          column(4,
                 textAreaInput(paste0("text_", i), label = paste("Text", i), rows = 3)
          )
        )
      })
    )
    
  })
  
  # Render the data table with a Delete button in each row.
  output$data_table <- renderDT({
    df <- values()
    df$Delete <- sprintf(
      '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'delete_row\', %d, {priority: \'event\'})">X</button>',
      seq_len(nrow(df))
    )
    datatable(df,
              escape = FALSE,
              rownames = FALSE,
              options = list(autoWidth = TRUE, pageLength = num_inputs()))
  }, server = FALSE)
  
  # When a Delete button is clicked, clear that row’s inputs.
  observeEvent(input$delete_row, {
    i <- input$delete_row
    updateTextInput(session, paste0("time_", i), value = "")
    updateTextAreaInput(session, paste0("text_", i), value = "")
  })
  
  # Download handler for exporting the table to an Excel file.
  output$download_data <- downloadHandler(
    filename = function() {
      paste("table_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      df <- values()
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      writeData(wb, "Data", df)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # page 2 ----
  
  output$PreviousDiets_ui <- renderUI({
    req(credentials()$user_auth)
    
    fluidPage(
      fluidRow(
        h1("user input gelecek")
      ),
      fluidRow(
        column(width = 6,
               h1("previous data")
               #DTOutput("previousData")
               ),
        column(width = 6,
               h1("notlar gelecek")
               )
      )
    )
    
  })
  
  # Kalori Hesaplama ----
  
  output$CalorieCalculator_ui <- renderUI({
    req(credentials()$user_auth)
    
    fluidPage(
      fluidRow(
        box(
          title = "Input Fields", 
          width = 6, 
          solidHeader = TRUE, 
          status = "primary",
          height = 500,
          numericInput(inputId = "Sut",
                       label = "Porsiyon - Sut",
                       value = 0),
          numericInput(inputId = "Et",
                       label = "Porsiyon - Et",
                       value = 0),
          numericInput(inputId = "Ekmek",
                       label = "Porsiyon - Ekmek",
                       value = 0),
          numericInput(inputId = "Sebze",
                       label = "Porsiyon - Sebze",
                       value = 0),
          numericInput(inputId = "Meyve",
                       label = "Porsiyon - Meyve",
                       value = 0),
          numericInput(inputId = "Yag",
                       label = "Porsiyon - Yağ",
                       value = 0)
          ),
        box(
          title = "Output Fields", 
          width = 6, 
          solidHeader = TRUE, 
          status = "primary",
          height = 500,
          dataTableOutput(outputId = "CalorieTable")
        )
      ),
      br(),
      fluidRow(
        infoBoxOutput(width = 3, "infoKarbonhidrat"),
        infoBoxOutput(width = 3, "infoProtein"),
        infoBoxOutput(width = 3, "infoYag"),
        infoBoxOutput(width = 3, "infoTotal"),
      )
    )
    
  })
  
  CalorieData <- reactive({ 
    
    Porsiyon <- 
      c(input$Sut,
        input$Et,
        input$Ekmek,
        input$Sebze,
        input$Meyve,
        input$Yag)
    
    data.frame(
      Karbonhidrat = Porsiyon * c(9,0,15,7,12,0),
      Protein = Porsiyon * c(6,6,2,2,0,0),
      Yag = Porsiyon * c(6,5,0,0,0,5)
    )
  })
  
  output$CalorieTable <- renderDataTable({
    CalorieData() %>% 
      mutate(Rownames = c("Sut","Et","Ekmek","Sebze","Meyve","Yag")) %>% 
      set_rownames(.$Rownames) %>% select(-Rownames)
  })
  
  output$infoKarbonhidrat <- renderInfoBox({
    infoBox("Karbonhidrat", 
            paste("Kalori:", 4 * sum(CalorieData()$Karbonhidrat),"Kalori"),
            paste("Yuzde:", round(
              100 * 4 * sum(CalorieData()$Karbonhidrat) / sum(colSums(CalorieData())* c(4,4,9)),
              3),"%"))})
  output$infoProtein <- renderInfoBox({
    infoBox("Protein", 
            paste("Kalori:", 4 * sum(CalorieData()$Protein),"Kalori"),
            paste("Yuzde:", round(
              100 * 4 * sum(CalorieData()$Protein) / sum(colSums(CalorieData())* c(4,4,9)),
              3),"%"))
  })
  output$infoYag <- renderInfoBox({
    infoBox("Yag", 
            paste("Kalori:", 9 * sum(CalorieData()$Yag),"Kalori"),
            paste("Yuzde:", round(
              100 * 9 * sum(CalorieData()$Yag) / sum(colSums(CalorieData()) * c(4,4,9)),
              3),"%"))
  })
  output$infoTotal <- renderInfoBox({
    infoBox("Total Kalori", 
            paste("Total:", sum(colSums(CalorieData()) * c(4,4,9)), "Kalori"))
  })
  
  
  # BMI Hesaplama ----
  
  output$BMICalculator_ui <- renderUI({
    req(credentials()$user_auth)
    
    fluidPage(
      fluidRow(
        box(
          title = "Input Fields", 
          width = 6, 
          solidHeader = TRUE, 
          status = "primary",
          height = 300,
          numericInput(inputId = "Height", 
                       label = "Boy - Santimetre cinsinden",
                       value = 180),
          numericInput(inputId = "Weight",
                       label = "Ağırlık - Kilogram cinsinden",
                       value = 70)
        ),
        box(
          title = "Output Fields", 
          width = 6, 
          solidHeader = TRUE, 
          status = "primary",
          height = 300,
          br(),
          p(h3("BMI Hesaplama")),
          br(),
          br(),
          textOutput("bmi_out"), 
        )
      ),
      fluidRow(
        highchartOutput("BMI_Chart")
      )
    )
    
  })
  
  bmi <- reactive({
    round(input$Weight / (input$Height^2) * 10000, 3)
  })
  
  output$bmi_out <- renderText({
    paste("BMI:", bmi())
  })
  
  output$BMI_Chart <- renderHighchart({
    
    df <- NHANES[!is.na(NHANES$BMI), ]
    
    # Compute the density
    d <- density(df$BMI)
    
    bmiValue <- bmi()
    bmiValue_density <- as.numeric(approx(d$x, d$y, xout = bmiValue)$y)
    
    # Create an interactive density chart
    highchart() %>%
      hc_xAxis(title = list(text = "BMI")) %>%
      hc_yAxis(title = list(text = "BMI Yoğunluk")) %>%
      hc_add_series(
        data = list_parse2(data.frame(x = d$x, y = d$y)),
        type = "spline",
        name = "BMI Density",
        color = "#2b908f",
        marker = list(enabled = FALSE)
      ) %>%
      hc_add_series(
        data = list(c(bmiValue, bmiValue_density)),
        type = "scatter",
        name = "Value",
        color = "red",
        marker = list(symbol = "cross", radius = 8)
      )
  })

  
}

# Run the app  -----------------------

# run app ----
cmd_ipconfig <- system("ipconfig", intern=TRUE)
cmd_ipv4 <- cmd_ipconfig[grep("IPv4", cmd_ipconfig)]
host <- gsub(".*? ([[:digit:]])", "\\1", cmd_ipv4)

runApp(shinyApp(ui = ui, server = server),
       port = 443,
       host = host,
       launch.browser = TRUE)

#shinyApp(ui = ui, server = server, 
#         options = list(launch.browser = TRUE,
#                        port = getOption("shiny.port", 5554),
#                        host = getOption("shiny.host", "127.0.0.1")))
#