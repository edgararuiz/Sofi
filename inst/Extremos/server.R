library(shiny)
source("puntosextremos.R")


shinyServer(function(input, output) {
  
  #________________________________________________________________________
  #Leer Datos y crear datos
  #_______________________________________________________________________
  
  datasetInput <- reactive({
    read.csv(input$file$datapath, header=input$header, 
             sep=input$sep, quote=input$quote)
  })
  
  output$registros<-renderPrint({
    if (is.null(input$file)) return(":-)")
    dim(datasetInput())[1]})
  
  output$VNA <- renderUI({
    if (is.null(input$file))
      return(NULL)
    checkboxGroupInput("NAs", 
                       label = h4("Variables a cambiar"), 
                       choices = c(colnames(datasetInput())),
                       selected = c(colnames(datasetInput()[3]),colnames(datasetInput()[4])))
  })
  
  values <- reactiveValues()
  
  datasetInput1 <- reactive({
    if(input$updat1==0){values$a<-datasetInput()
                         Tem<-datasetInput()
                         return(Tem)}
    input$updat1
    values$a<-isolate(datasetInput())#quitar si se quieren guardar los cambios
    values$a<-isolate(PonNA(values$a,input$NAs))
    return(values$a)
  })
  
  output$val<-renderPrint({dim(datasetInput2()$vars.ext)[2]})
  
  datasetInput2 <- reactive({
    if(input$updat2==0) return()
    input$updat2
    Tam<-isolate(PEconNA(as.data.frame(datasetInput1()),2,input$ns))
    return(Tam)
  })
  
  datasetInput20 <- reactive({
    if(input$updat2==0) return()
    Tam<-datasetInput2()
    Tam$Transforma<-NULL
    return(Tam)
  })
  
  datasetInput3 <- reactive({
    if(input$updat2==0) return()
    Ext1<-datasetInput2()$vars.ext[,1]
    Ext1[is.na(Ext1)]<-" "
    Ext2<-datasetInput2()$vars.ext[,2]
    Ext2[is.na(Ext2)]<-" "
    Ext3<-datasetInput2()$vars.ext[,3]
    Ext3[is.na(Ext3)]<-" "
    Tam<-data.frame(datasetInput2()$regID,Ext1,Ext2,Ext3)
    return(Tam)
  })
  
  #___________________________________________________________________________
  #Tablas
  #___________________________________________________________________________
  
  output$tabla1 <- renderTable({
    if (is.null(input$file)) return(NULL)
    head(datasetInput(), n = input$obs)
  })
  
  output$tabla2 <- renderTable({
    if(is.null(input$file)) {return(NULL)}
    head(datasetInput1(), n = input$obs)
  })
  
  output$tabla30 <- renderTable({
    if(input$updat2==0) return()
    datasetInput2()$Transforma
  })
  
  output$tabla31 <- renderTable({
    if(input$updat2==0) return()
    datasetInput2()$regID
  })
  
  output$tabla32 <- renderTable({
    if(input$updat2==0) return()
    datasetInput2()$valores
  })
  
  output$tabla33 <- renderTable({
    if(input$updat2==0) return()
    datasetInput2()$norm.estan
  })
  
  output$tabla34 <- renderTable({
    if(input$updat2==0) return()
    datasetInput2()$vars.ext
  })
  
  #______________________________________________________________________
  #Grafica
  #________________________________________________________________________
  
  output$plot1 <- renderPlot({
    if(input$updat2==0) return()
    graNormPE(datasetInput2(),input$graf)
  })
   
  #______________________________________________________________________
  #Guardar Datos
  #_______________________________________________________________________
  
  output$DescarResum <- downloadHandler(
    filename = function() {
      paste('Dat',input$file[1], Sys.Date(),'.csv', sep='-') 
    },
    content = function(file) {
      write.csv(datasetInput20(), file)
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Rep',input$file[1],Sys.Date(), '.pdf', sep='-')
    },
    content = function(file) {
      rnw <- normalizePath('Reporte.Rnw')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      library(knitr)
      out <- knit2pdf(rnw)
      file.rename(out, file)
    }
  )
  
})