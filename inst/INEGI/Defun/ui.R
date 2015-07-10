#library(shiny)
#library(shinythemes)
options(shiny.deprecation.messages=FALSE)
shinyUI(navbarPage("Defunciones",#theme = shinytheme("readable"),
####_____________________________________________________________
#___________________________Etapa 1______________________________
####________________________________________________________________                   
tabPanel("Etapa 1",icon = icon("random"),
    #source(system.file("INEGI/Defun/General/IU_Etapa1.R", package="Sofi"),local=T,encoding="UTF-8")$value
    #source(system.file("INEGI/Defun/General/IU_Etapa1.R", package="Sofi"),local=T)$value
    source('./General/IU_Etapa1.R',local=T,encoding="UTF-8")$value
),
####_____________________________________________________________
#___________________________Etapa 2______________________________
####________________________________________________________________
tabPanel("Etapa 2",
    source(system.file("INEGI/Defun/General/IU_Etapa2.R", package="Sofi"), local=T, encoding="UTF-8")$value
),#tabPanel("Etapa 2 y 3",
####_____________________________________________________________
#___________________________Etapa 3______________________________
####________________________________________________________________
tabPanel("Etapa 3",
#         source(system.file("INEGI/Defun/General/IU_Etapa2.R", package="Sofi"),local=T)$value
sidebarLayout(
  
  sidebarPanel(
    h4("Opciones  para el registro de los códigos sugeridos por el codificador experto."),
    uiOutput("Et3_Num_reg")
    
    
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    #plotOutput("distPlot")
    flowLayout(
      wellPanel(h5("NOREG1:"),verbatimTextOutput("E3NORE")),
      wellPanel(h5("Foliocer:"),verbatimTextOutput("E3Foli")),
      wellPanel(h5("Sexo:"),verbatimTextOutput("E3Sexo")),
      wellPanel(h5("Edad:"),verbatimTextOutput("E3Edad")),
      wellPanel(h5("Descr_lin1:"),verbatimTextOutput("E3Desc1")),
      wellPanel(h5("Descr_lin2:"),verbatimTextOutput("E3Desc2")),
      wellPanel(h5("Descr_lin3:"),verbatimTextOutput("E3Desc3")),
      wellPanel(h5("Descr_lin4:"),verbatimTextOutput("E3Desc4")),
      wellPanel(h5("Descr_lin5:"),verbatimTextOutput("E3Desc5")),
      wellPanel(h5("Duration1:"),verbatimTextOutput("E3Dura"))
      )
    
   
  ),
  position = "right"
)
),#tabPanel("Etapa 2 y 3",


####_____________________________________________________________
#___________________________Etapa 4 y 5______________________________
####________________________________________________________________
tabPanel("Etapa 4 y 5",
         #fluidPage(
    source(system.file("INEGI/Defun/General/IU_Etapa4.R", package="Sofi"),local=T,encoding="UTF-8")$value
)#tabPanel("Etapa 4 y 5",

))
