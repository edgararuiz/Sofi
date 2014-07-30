library(shiny)
library(foreign)
library(sampling)
source("helpers.R")

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output) {
  
  ##########Ejemplo
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  ####

##_____________________________________________________________
# Leer Datos
##_____________________________________________________________
  
  datasetInput1 <- reactive({
    read.dbf(input$file1$datapath)
  })
  
  output$NomID <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("ID","Seleccionar ID", 
                choices=c("",colnames(datasetInput1())),selected="")
  })
  
  output$NomCod <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("Codigo","Seleccionar c\u00F3digos", 
                choices=c("",colnames(datasetInput1())),selected="")
  })
  
  datasetInput2 <- reactive({
    read.csv(input$file2$datapath, header=input$header, 
             sep=input$sep, quote=input$quote)
  })
  
  datasetInput3 <- reactive({
    if (is.null(datasetInput1()))
      return(NULL)
    input$updat1
    Tam<-isolate(Ordenar(IDm=datasetInput1()[,input$ID],
                 CausaD=datasetInput1()[,input$Codigo]))
    return(Tam)
  })
  
  datasetInput4 <- reactive({
    if (is.null(datasetInput2()))
      return(NULL)
    dat<-data.frame(datasetInput2())
    N<-datasetInput3()[[2]][,2]
    input$updat2
    MuestraGr<-isolate(OptFact(dat$Capitulo,N,dat$n))
    Muestra<-merge(datasetInput3()[[1]], MuestraGr, by.x="Id", 
                   by.y="NT")
    return(Muestra)
  })
 
  datasetInput5 <- reactive({
    if(input$updat1==0) return()
    dat<-as.data.frame(datasetInput2())
    Npob<-datasetInput3()[[2]][,2]
    Datos<-data.frame(dat,Npob)
    return(Datos)
  })
  
values <- reactiveValues()

  datasetInput51 <- reactive({
    if(input$updat3==0) {
      dat<-as.data.frame(datasetInput2())
      Npob<-datasetInput3()[[2]][,2]
      Datos<-data.frame(dat,Npob)
      values$a<-Datos
      return(Datos)}
      values$a[input$CapEcu,2]<-input$bw_Error
      values$a[input$CapEcu,3]<-input$bw_P
    #input$updat3
      values$a[input$CapEcu,4]<-optn(values$a[input$CapEcu,5],input$bw_P,input$bw_Error)
    
    #dat2<-data.frame(dat,dat[,4])
    #input$nmanual
    #Npob<-datasetInput3()[[2]][,2]
    #Datos<-data.frame(dat2,Npob)
    return(values$a)
  })
  
  datasetInput52 <- reactive({
    if(input$updat4==0) {
      dat<-as.data.frame(datasetInput2())
      Npob<-datasetInput3()[[2]][,2]
      Datos<-data.frame(dat,Npob)
      values$b<-Datos
      return(Datos)}
    #dat<-datasetInput5()
    #input$updat4
    #dat[input$CapMod,4]<-isolate(input$nmanual)
    values$b[input$CapMod,4]<-input$nmanual
    #dat2<-data.frame(dat,dat[,4])
    #input$nmanual
    #Npob<-datasetInput3()[[2]][,2]
    #Datos<-data.frame(dat2,Npob)
    return(values$b)
  })

  datasetInput6 <- reactive({
    if (is.null(datasetInput4()))
      return(NULL)
    dat<-datasetInput4()
    Datos<-dat[,input$show_vars, drop = FALSE]
    return(Datos)
  })
  
  datasetInput7 <- reactive({
    if (is.null(datasetInput6()))
      return(NULL)
    dat<-datasetInput6()
    Datos<-dat[dat$EnMuestra==1,]
    return(Datos)
  })
  
  
  #_______________________________________________________________
  #Tablas
  #_______________________________________________________________
  output$tabla1 <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    head(datasetInput1(), n = input$obs)
  })
  
  output$tabla2 <- renderTable({
    if (is.null(input$file2))
      return(NULL)
    datasetInput2()
  })
  
  output$tabla5 <- renderTable({
    if(input$updat1==0) return()
    datasetInput5()
  })
  
  output$tabla51 <- renderTable({
    if(input$updat1==0) return()
    datasetInput51()
  })
  
  output$tabla52 <- renderTable({
    if(input$updat1==0) return()
    datasetInput52()
  })
  
  output$tabla4 <- renderDataTable({
    if(input$updat2==0) return()
    datasetInput6()
  }, options = list(aLengthMenu = c(10, 30, 50), 
                    iDisplayLength = 10))
    
  #options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5
  #Guardar Datos
  #
  output$DescarResum <- downloadHandler(
    filename = function() {
      paste('Resumen',input$file1[1], Sys.Date(), '.csv', sep='_') 
    },
    content = function(file) {
      write.csv(datasetInput5(), file)
    }
  )
  
  output$DescarMuestra <- downloadHandler(
    filename = function() {
      paste('Muestra',input$file1[1], Sys.Date(), '.zip', sep='_') 
    },
    content = function(file) {
      if (input$mues){write.dbf(datasetInput7(), "Muestra.dbf")}
      else{write.dbf(datasetInput6(), "Muestra.dbf")}
      zip(zipfile='fbCrawlExport.zip', files="Muestra.dbf")
      file.copy("fbCrawlExport.zip", file)
    }
  )
})
