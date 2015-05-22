
library(shiny)
library(shinyjs)

shinyUI(fluidPage(
  title = "Sofi",
  tags$head(includeCSS(file.path('www', 'style.css'))),   
  useShinyjs(),
  
  fluidRow(id = "title-row",
    column(12,
      h2("Pruebas para Sofi",
        a("(link)", href = "http://www.inegi.info/sofi")
      ),
      #h4("ggMarginal lets you add marginal plots to ggplot2 (finally!)"),
      h4(tags$i("El código fuente de esta aplicación esta",
         a("en GitHub", href = "https://github.com/daattali/ggExtra/tree/master/inst/examples/ggMarginal"),
         HTML("&bull;"), "por", a("Dean Attali", href = "http://deanattali.com")))
    )
  ),
  
  div(id = "loading-content", h2("Cargando...")),
  fluidRow(id = "app-content",
    column(3, wellPanel(
      class = "settings",
      h3(class = "settings-title", "Trama principal"),
      uiOutput("dataset_select"),
      uiOutput("x_var_select"),
      uiOutput("y_var_select"),
      sliderInput("font_size", "Tamaño de fuente", 0, 50, 15, 1)
    )),
    
    column(3, wellPanel(
      class = "settings",
      h3(class = "settings-title", "Parcelas marginales"),
      checkboxInput("show_marginal", "Mostrar parcelas marginales", F),
      
      div(id = "marginal-settings",
        selectInput("type", NULL, c("density", "histogram")),
        selectInput("margins", "¿Qué márgenes?", c("both", "x", "y")),
        sliderInput("size",
                    "Relación de tamaño de trama principal: parcelas marginales",
                    1, 5, 5, 0.5),
        selectInput("marginCol", "Color de trama Marginal", colours(), "black"),
        conditionalPanel(
          condition = "input.type == 'histogram'",
          selectInput("marginFill", "Marginal plot fill colour", colours(), "grey")
        )
      )      
    )),
    
    column(6,
      plotOutput("plot"),
      pre(id = "code")
    )
  )
))