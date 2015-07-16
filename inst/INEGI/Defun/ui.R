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
    wellPanel(h5("Total de registros:"),verbatimTextOutput("E3Tam")),
   
    uiOutput("Et3_Num_reg"),
    uiOutput("Et3_Causa"),
    
    radioButtons("Cod_Cor", "Radio buttons:",
                 c("label 1" = "option1",
                   #"label 2" = "option2",
                   "label 3" = "option3",
                   "label 4" = "option4")),
    textInput("Et3_inText",  "Text input:", value = "start text"),
    
    actionButton("Bot_RAM", "RAM"),
    actionButton("Bot_Ant", "Anterior"),
    actionButton("Bot_Sig", "Siguiente"),
    
    wellPanel(h5("COD_SEL:"),verbatimTextOutput("E3COD")),
    actionButton("Bot_Guar", "DISCO"),
    wellPanel(h5("Nulos:"),verbatimTextOutput("E3Nul"))
    
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    #plotOutput("distPlot")
    titlePanel("Descripciones"),
    flowLayout(
      wellPanel(h5("NOREG1:"),verbatimTextOutput("E3NORE")),
      wellPanel(h5("Foliocer:"),verbatimTextOutput("E3Foli")),
      wellPanel(h5("Sexo:"),verbatimTextOutput("E3Sexo")),
      wellPanel(h5("Edad:"),verbatimTextOutput("E3Edad")),
      wellPanel(h5("Descr_lin1:"),verbatimTextOutput("E3Desc1")),
      wellPanel(h5("TXT_CODIA:"),verbatimTextOutput("E3t_CoA")),
      wellPanel(h5("Descr_lin2:"),verbatimTextOutput("E3Desc2")),
      wellPanel(h5("TXT_CODIB:"),verbatimTextOutput("E3t_CoB")),
      wellPanel(h5("Descr_lin3:"),verbatimTextOutput("E3Desc3")),
      wellPanel(h5("TXT_CODIC:"),verbatimTextOutput("E3t_CoC")),
      wellPanel(h5("Descr_lin4:"),verbatimTextOutput("E3Desc4")),
      wellPanel(h5("TXT_CODID:"),verbatimTextOutput("E3t_CoD")),
      wellPanel(h5("Descr_lin5:"),verbatimTextOutput("E3Desc5")),
      wellPanel(h5("TXT_CODII:"),verbatimTextOutput("E3t_CoI")),
      wellPanel(h5("Duration1:"),verbatimTextOutput("E3Dura")),
      wellPanel(h5("CAUSADEF:"),verbatimTextOutput("E3CAUS"))
      )
    #DT::dataTableOutput("Etapa3Tabla1", width = 900)
    
   
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
