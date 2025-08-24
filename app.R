library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyjs)

source("modules/new_diet_module.R")
source("modules/previousDiets.R")
source("modules/calorieCalculator.R")
source("modules/BMICalculator.R")

# sample logins dataframe with passwords hashed by sodium package
user_base <- tibble(
  user = c("admin", "user2"),
  password = sapply(c("admin", "pass2"), sodium::password_store), 
  permissions = c("admin", "standard"),
  name = c("admin", "User Two")
)

ui <- dashboardPage(
  dashboardHeader(title = "Diyet Uygulaması"),
  dashboardSidebar(
    collapsed = TRUE, sidebarMenuOutput("sidebar")
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyauthr::loginUI("login"),
    
    tabItems(
      tabItem("new_diet", newDietUI("new_diet1")),
      tabItem("previousDiets", previousDietsUI("previous_diets")),
      tabItem("calorie_calc", calorieCalculatorUI("calc1")),
      tabItem("BMICalculator", bmiCalculatorUI("BMI_Calculator"))
    )
    )
)

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
      menuItem("Yeni Diyet", tabName = "new_diet", icon = icon("person")),
      menuItem("Geçmiş Diyetler", tabName = "previousDiets", icon = icon("history")),
      menuItem("Kalori Hesaplayıcı", tabName = "calorie_calc", icon = icon("calculator")),
      menuItem("BMI Hesaplama", tabName = "BMICalculator", icon = icon("weight-scale", lib = "font-awesome"))
    )
  })
  
  # Yeni Diyet module
  newDietServer("new_diet1")
  
  # Eski Diyetler
  previousDietsServer("previous_diets")
  
  # Kalori hesaplayici
  calorieCalculatorServer("calc1")
  
  bmiCalculatorServer("BMI_Calculator")
}

shinyApp(ui, server)
