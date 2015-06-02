library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Calculadora de Distribución"),
  
  sidebarPanel(
    #radio button or dropdown?

    selectInput(inputId = "dist_CalDis",
                label = "Distribución:",
                choices = c("Normal"      = "rnorm",
                            "Binomial"    = "rbinom",
                            "t"           = "rt",
                            "F"           = "rf",
                            "Chi-Cuadrado" = "rchisq"),
                selected = "rnorm"),
    

    br(),

    uiOutput("mean_CalDis"),
    uiOutput("sd_CalDis"),
    uiOutput("df1_CalDis"),
    uiOutput("df2_CalDis"),
    uiOutput("n_CalDis"),
    uiOutput("p_CalDis"),

    br(),
    br(),

    helpText("Modelo:"),
    h4(textOutput("model_CalDis"), align = "center"),
    br(),

    uiOutput("tail_CalDis"),
    uiOutput("lower_bound_CalDis"),
    uiOutput("upper_bound_CalDis"),
    

    uiOutput("a_CalDis"),
    uiOutput("b_CalDis"),
    
    br(),
    
    withMathJax(),  # include the MathJax library
    helpText("Some math here $$Y = \\beta_0 + \\beta_1 x + \\epsilon$$"),
    #selectInput("x", "Construir un modelo de regresión de mpg en contra:",choices = names(mtcars)[-1]),
    
    helpText(a(href="https://duke.qualtrics.com/SE/?SID=SV_3L8WjmwQo32cVk9", target="_blank", "Rate this app!")),
    helpText(a(href="https://github.com/ShinyEd/ShinyEd/tree/master/dist_calc", target="_blank", "View code")),
    helpText(a(href="http://stat.duke.edu/~mc301/shiny/applets.html", target="_blank", "Check out other apps")),
    helpText(a(href="https://www.coursera.org/course/statistics", target="_blank", "Want to learn more for free?"))),
  
  
  
  mainPanel(
    #uiOutput('report'),
    #div(h4(textOutput("Peso_Est"), align = "center")),
    h3("Peso de estudiantes",align = "center"),
    h4(textOutput("Peso_Est")),
    p(textOutput("status1"),style="font-weight=500; color: #000000;"),
    h5(textOutput("status2"),style="font-weight=500; color: #00CC00;"),
    h5(textOutput("status3"),style="font-weight=500; color: #FF0000;"),
    br(),
    #actionButton("submit","Enviar"),
    actionButton("newdat","Nuevos datos"),
    h4(textOutput("Error_CalDis")),
    h4(textOutput("area_CalDis"), align = "center"),
    plotOutput("plot_CalDis"),
    #textOutput("area_CalDis")
    helpText("Some math here, $$Y = \\beta_0 + \\beta_1 x + \\epsilon$$"),
    br()
    
    #uiOutput('report')
  )
))