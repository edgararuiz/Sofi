library(shiny)

shinyUI(fluidPage(#theme = "bootstrap02.css",
  titlePanel("Etapa 1"),
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
                         selectInput("CapEcu","Capitulo que desea modificar:", 
                                     choices=c(1:20),selected="1"),
                         tags$hr(),
                         sliderInput(inputId = "bw_Error",
                                     label = "Valor para Error:",
                                     min = 0, max = 0.2, value = .04, step = 0.01),
                         sliderInput(inputId = "bw_P",
                                     label = "Valor para P:",
                                     min = 0, max = 1, value = .5, step = 0.05)
        ),
        conditionalPanel(condition = 'input.En === "man"',
                         actionButton("updat4", "Renovar"),
                         selectInput("CapMod","Capitulo que desea modificar:", 
                                     choices=c(1:20)),
                         tags$hr(),
                         numericInput("nmanual", "Introducir valores para n:", 
                                      100)                
        ),
        tags$hr(),
        actionButton("updat2", "Obtener Muestra"),
        tags$hr(),
        downloadButton('DescarResum', 'Guardar')
      ),
      
      conditionalPanel(
        'input.Etap01 === "Muestra"',
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
      ),
      conditionalPanel(
        'input.Etap01 === "Ejemplo"',
        helpText('Elige las variables a exhibir'),
        selectInput('xcol', 'X Variable', names(iris)),
        selectInput('ycol', 'Y Variable', names(iris),
                    selected=names(iris)[[2]]),
        numericInput('clusters', 'Numero de Cluster', 3,
                     min = 1, max = 9)
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
                                  h4("Tabla de Resumen Archivo"),
                                  tableOutput('tabla5')
                 ),
                 conditionalPanel(condition = 'input.En === "ecu"',
                                  h4("Tabla de Resumen Ecuaciones"),
                                  tableOutput('tabla51')
                 ),
                 conditionalPanel(condition = 'input.En === "man"',
                                  h4("Tabla de Resumen Manual"),
                                  tableOutput('tabla52')
                 )
                 
                 ),
        
        tabPanel("Muestra",
                 h4("Tabla con la muestra para la Etapa 1"),
                 dataTableOutput('tabla4')
                ),
        tabPanel("Ejemplo",
                 h4("Gráfica usando K medias"),
                 plotOutput('plot1')
        )
              )
          )
  )
))
