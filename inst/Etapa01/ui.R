library(shiny)

shinyUI(navbarPage("Defunciones",
                   tabPanel("Etapa 1",
  #fluidPage(#theme = "bootstrap02.css",
  #titlePanel("Etapa 1"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.Etap01 === "Datos"',
        fileInput('file1', 'Archivo de códigos (en dbf)',
                  accept=c('.dbf')),
        numericInput("obs", "Primeros casos del archive:", 20),
        tags$hr(),
        fileInput('file2', 'Archivo de datos (texto o csv)',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        
        checkboxInput('header', 'Encabezado', TRUE),
        radioButtons('sep', 'Separado por:',
                     c(Coma=',',
                       Puntoycoma=';',
                       Tabulador='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        tags$hr(),
        p('Para ver un ejemplo de cómo están organizados los',
          'archivos .dbf y .cvs, puede descargar estos ejemplos',
          a(href = 'BaseEtapa01.dbf', 'BaseEtapa01.dbf'), 'o',
          a(href = 'DatCap.csv', 'DatCap.csv'),
          'y hacer pruebas con ellos.'
        ),
        p('Nota: El archivo de códigos debe ser guardado en',
          'el formado', code("dBASE IV (DBF)"), 'y con solo las columnas', 
          'de ID y código si son más de 300,000 registros.')
      ),
      
      conditionalPanel(
        'input.Etap01 === "Resumen"',
        helpText('Elegir las variables a utilizar'),
        uiOutput("NomID"),
        uiOutput("NomCod"),
        actionButton("updat1", "Obtener Población"),
        tags$hr(),
        radioButtons("En", "De donde tomar n:",
                     c("Archivo" = "arc",
                       "Ecuación" = "ecu",
                       "Manual" = "man")),
        conditionalPanel(condition = 'input.En === "ecu"',
                         actionButton("updat3", "Renovar"),
                         tags$hr(),
                         selectInput("CapEcu","Capitulo que desea modificar:", 
                                     choices=c(1:20),selected="1"),
                         sliderInput(inputId = "bw_Error",
                                     label = "Valor para Error:",
                                     min = 0, max = 0.2, value = .04, step = 0.01),
                         sliderInput(inputId = "bw_P",
                                     label = "Valor para P:",
                                     min = 0, max = 1, value = .5, step = 0.05)
        ),
        conditionalPanel(condition = 'input.En === "man"',
                         actionButton("updat4", "Renovar"),
                         tags$hr(),
                         selectInput("CapMod","Capitulo que desea modificar:", 
                                     choices=c(1:20)),
                         numericInput("nmanual", "Introducir valores para n:", 
                                      100)                
        ),
        #tags$hr(),
        downloadButton('DescarResum', 'Guardar'),
        tags$hr()
      ),
      
      conditionalPanel(
        'input.Etap01 === "Muestra"',
        actionButton("updat2", "Obtener Muestra"),
        tags$hr(),
        helpText('Elige las variables a exhibir'),
        checkboxGroupInput('show_vars', 'Elegir:',
                           c("Id" = "Id",
                             "ID Original" = "IDm",
                             "Código de defunción" = "CausaD",
                             "CapAut" = "CapAut",
                             "Capit" = "Capit",
                             "FactorExp" = "FactorExp",
                             "En Muestra" = "EnMuestra"),
                           selected = c("Id","IDm","CausaD","CapAut","Capit","FactorExp","EnMuestra")),
        tags$hr(),
        checkboxInput("mues","Solo los elementos de la muestra",value = T),
        downloadButton('DescarMuestra', 'Guardar')
      )
      ),
    
    mainPanel(
      tabsetPanel(
        id = 'Etap01',
        tabPanel("Datos",    
          fluidRow(
            column(4,offset = 1,h4("Tabla de códigos"),
                tableOutput('tabla1')),
            column(4,offset = 1,h4("Tabla de Datos"),
                tableOutput('tabla2')
                  )
                  )
                ),
        
        tabPanel("Resumen",
                 conditionalPanel(condition = 'input.En === "arc"',
                                  fluidRow(
                                    column(4,h4("Tabla de Resumen Archivo"),
                                           tableOutput('tabla5')),
                                    column(7,offset = 1,h5("Cantidad de registros para la muestra:"),
                                           verbatimTextOutput("num5"))
                                  )
                 ),
                 conditionalPanel(condition = 'input.En === "ecu"',
                                  fluidRow(
                                    column(5,h4("Tabla de Resumen Ecuaciones"),
                                           tableOutput('tabla51')),
                                    column(5,offset = 1,h5("Cantidad de registros para la muestra:"),
                                           verbatimTextOutput("num51"))
                                  )
                 ),
                 conditionalPanel(condition = 'input.En === "man"',
                                  fluidRow(
                                    column(5,h4("Tabla de Resumen Manual"),
                                           tableOutput('tabla52')),
                                    column(5,offset = 1,h5("Cantidad de registros para la muestra:"),
                                           verbatimTextOutput("num52"))
                                  )
                 )
                 ),
        
        tabPanel("Muestra",
                 h4("Tabla con la muestra para la Etapa 1"),
                 dataTableOutput('tabla4')
                )
              )
          )
  )
#)
),

#___________________________Etapa 2______________________________
tabPanel("Etapa 2",
         #fluidPage(
         sidebarLayout(
           sidebarPanel(
             conditionalPanel(
               'input.Etap02 === "Datos"',
               fileInput('Etapa2file1', 'Archivo de códigos (en dbf)',
                         accept=c('.dbf')),
               #numericInput("obs", "Primeros casos del archive:", 20),
               tags$hr()
               
             ),
             
             conditionalPanel(
               'input.Etap02 === "Revisión"',
               helpText('Elegir las variables a utilizar'),
               uiOutput("Etap2CausaA"),
               uiOutput("Etap2Causa1"),
               uiOutput("Etap2Causa2"),
               actionButton("E2updat1", "Evaluar"),
               tags$hr(),
               h6("Registros para revisión:"),
               verbatimTextOutput("RegRev"),
               tags$hr(),
               checkboxInput("RevGua","Solo los Registros para revisión",value = T),
               downloadButton('DescarRev', 'Guardar'),
               tags$hr()
             ),
             
             conditionalPanel(
               'input.Etap02 === "Tablas"',
               helpText('Elegir la tabla que decea ver'),
               tags$hr(),
               radioButtons("Tab", "Tipo de tabla:",
                            c("Totales por caso" = "Caso",
                              "Errores a 3 Dígitos" = "Er3D",
                              "Errores a 4 Dígitos" = "Er4D",
                              "Caso 4 Mismo Capítulo" = "C4MC",
                              "Caso 4 Diferente Capítulo" = "C4DF"
                              )),
               conditionalPanel(condition = 'input.Tab === "Caso"',
                                helpText('Si desea guardar la tabla de totales por caso:'),
                                downloadButton('DescarCaso', 'Guardar'),
                                tags$hr()
               ),
               conditionalPanel(condition = 'input.Tab === "Er3D"',
                                helpText('Si desea guardar la tabla de Errores a 3 Dígitos:'),
                                downloadButton('DescarEr3D', 'Guardar'),
                                tags$hr()             
               ),
               conditionalPanel(condition = 'input.Tab === "Er4D"',
                                helpText('Si desea guardar la tabla de Errores a 4 Dígitos:'),
                                downloadButton('DescarEr4D', 'Guardar'),
                                tags$hr()               
               ),
               conditionalPanel(condition = 'input.Tab === "C4MC"',
                                numericInput("nMis", "Mínimo de Errores:", 1),
                                helpText('Si desea guardar frecuencias para mismo capítulo:'),
                                downloadButton('DescarC4MC', 'Guardar'),
                                tags$hr()               
               ),
               conditionalPanel(condition = 'input.Tab === "C4DF"',
                                numericInput("nDif", "Mínimo de Errores:", 1),
                                helpText('Si desea guardar frecuencias para diferente capítulo:'),
                                downloadButton('DescarC4DF', 'Guardar'),
                                tags$hr()               
               ),
               tags$hr()
             )
             
           ),
           
           mainPanel(
             tabsetPanel(
               id = 'Etap02',
               tabPanel("Datos",    
                        h4("Tabla de Datos"),
                        dataTableOutput('Etapa2Tabla1')
               ),
               
               tabPanel("Revisión",
                        h4("Tabla de Revisión"),
                        dataTableOutput('Etapa2Tabla2')
               ),
               
               tabPanel("Tablas",
                        conditionalPanel(condition = 'input.Tab === "Caso"',
                                         h4("Tabla de Totales por Caso"),
                                         tableOutput('Etapa2Tabla31')
                                         #dataTableOutput('Etapa2Tabla34')
                        ),
                        conditionalPanel(condition = 'input.Tab === "Er3D"',
                                         h4("Tabla de Errores a 3 Dígitos"),
                                         tableOutput('Etapa2Tabla32')
                        ),
                        conditionalPanel(condition = 'input.Tab === "Er4D"',
                                         h4("Tabla de Errores a 4 Dígitos"),
                                         tableOutput('Etapa2Tabla33')
                        ),
                        conditionalPanel(condition = 'input.Tab === "C4MC"',
                                         h4("Frecuencias para mismo capítulo"),
                                         tableOutput('Etapa2TablaMis')
                        ),
                        conditionalPanel(condition = 'input.Tab === "C4DF"',
                                         h4("Frecuencias para diferente capítulo"),
                                         tableOutput('Etapa2TablaDif')
                        )
                        
                        
               )
               
             )
           )
         )
)

))
