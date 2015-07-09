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
tabPanel("Etapa 2 y 3",
    source(system.file("INEGI/Defun/General/IU_Etapa2.R", package="Sofi"),local=T,encoding="UTF-8")$value
),#tabPanel("Etapa 2 y 3",
####_____________________________________________________________
#___________________________Etapa 3______________________________
####________________________________________________________________
#tabPanel("Etapa 3",
#         source(system.file("INEGI/Defun/General/IU_Etapa2.R", package="Sofi"),local=T)$value
#),#tabPanel("Etapa 2 y 3",


####_____________________________________________________________
#___________________________Etapa 4 y 5______________________________
####________________________________________________________________
tabPanel("Etapa 4 y 5",
         #fluidPage(
    source(system.file("INEGI/Defun/General/IU_Etapa4.R", package="Sofi"),local=T,encoding="UTF-8")$value
)#tabPanel("Etapa 4 y 5",

))
