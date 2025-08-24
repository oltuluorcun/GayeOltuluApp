# ---- BMI Calculator Module ----

bmiCalculatorUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        title = "Input Fields", width = 6, solidHeader = TRUE, status = "primary", height = 300,
        numericInput(ns("Height"), "Boy - Santimetre", 180),
        numericInput(ns("Weight"), "Ağırlık - Kilogram", 70)
      ),
      box(
        title = "Output Fields", width = 6, solidHeader = TRUE, status = "primary", height = 300,
        br(), p(h3("BMI Hesaplama")), br(), br(),
        textOutput(ns("bmi_out"))
      )
    ),
    fluidRow(
      highchartOutput(ns("BMI_Chart"))
    )
  )
}

bmiCalculatorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ---- Reactive BMI calculation ----
    bmi <- reactive({
      req(input$Height, input$Weight)
      round(input$Weight / (input$Height^2) * 10000, 3)
    })
    
    output$bmi_out <- renderText({
      paste("BMI:", bmi())
    })
    
    output$BMI_Chart <- renderHighchart({
      df <- NHANES[!is.na(NHANES$BMI), ]
      d <- density(df$BMI)
      
      bmiValue <- bmi()
      bmiValue_density <- as.numeric(approx(d$x, d$y, xout = bmiValue)$y)
      
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
  })
}
