library(shiny)
library(foreign)
library(sampling)
library(INEGI)
source("helpers.R")
options(shiny.maxRequestSize=300*1024^2)

shinyServer(function(input, output) {

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
    if(input$En=="ecu"){
      dat<-data.frame(datasetInput51())
      values$c<-dat$n}
    else {if(input$En=="man"){
      dat<-data.frame(datasetInput52())
      values$c<-dat$n}
          else {dat<-data.frame(datasetInput2())
                values$c<-dat$n}}
    N<-datasetInput3()[[2]][,2]
    input$updat2
    MuestraGr<-isolate(OptFact(dat$Capitulo,N,values$c))
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
  
#________________________________
#Varios
#___________________________________
values <- reactiveValues()
output$num5<-renderPrint({
  if (input$updat1==0) return(":-)")
  sum(datasetInput5()[,4])})
output$num51<-renderPrint({
  if (input$updat3==0) return(":-)")
  sum(datasetInput51()[,4])})
output$num52<-renderPrint({
  if (input$updat4==0) return(":-)")
  sum(datasetInput52()[,4])})

#____________________________________
#_____________________________________

  datasetInput51 <- reactive({
    if(input$updat3==0) {
      dat<-as.data.frame(datasetInput2())
      Npob<-datasetInput3()[[2]][,2]
      Datos<-data.frame(dat,Npob)
      values$a<-Datos
      return(Datos)}
      values$a[input$CapEcu,2]<-input$bw_Error
      values$a[input$CapEcu,3]<-input$bw_P
      values$a[input$CapEcu,4]<-optn(values$a[input$CapEcu,5],input$bw_P,input$bw_Error)
    return(values$a)
  })
  
  datasetInput52 <- reactive({
    if(input$updat4==0) {
      dat<-as.data.frame(datasetInput2())
      Npob<-datasetInput3()[[2]][,2]
      Datos<-data.frame(dat,Npob)
      values$b<-Datos
      return(Datos)}
    values$b[input$CapMod,4]<-input$nmanual
    return(values$b)
  })

  datasetInput6 <- reactive({
    if (input$updat2==0)
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

  output$DescarResum <- downloadHandler(
    filename = function() {
      paste('Resumen',input$file1[1], Sys.Date(), '.csv', sep='_') 
    },
    content = function(file) {
      if(input$En=="ecu"){write.csv(datasetInput51(), file)}
      else {if(input$En=="man"){write.csv(datasetInput52(), file)}
      else {write.csv(datasetInput5(), file)}}
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

#_______________________________________________________________
#_______________________________________________________________
#Etapa 2
#_______________________________________________________________
#_______________________________________________________________

#Datos
Etapa2Data1 <- reactive({
  read.dbf(input$Etapa2file1$datapath)
})


output$Etapa2Tabla1 <- renderDataTable({
  if (is.null(input$Etapa2file1)) return(NULL)
  Etapa2Data1()
},options = list(aLengthMenu = c(5, 10, 50), 
                 iDisplayLength = 5))

output$Etap2CausaA <- renderUI({
  if (is.null(input$Etapa2file1)) return(NULL)
  selectInput("E2CA","Autom\u00e1tico (generalmente CAUSADEF)", 
              choices=c("",colnames(Etapa2Data1())),selected="")
})

output$Etap2Causa1 <- renderUI({
  if (is.null(input$Etapa2file1)) return(NULL)
  selectInput("E2C1","Codificador 1 (generalmente RECODCBD)", 
              choices=c("",colnames(Etapa2Data1())),selected="")
})

output$Etap2Causa2 <- renderUI({
  if (is.null(input$Etapa2file1)) return(NULL)
  selectInput("E2C2","Codificador 2 (generalmente RECODCBD2)", 
              choices=c("",colnames(Etapa2Data1())),selected="")
})


Etapa2Data2 <- reactive({
  if (input$E2updat1==0) return(NULL)
  input$E2updat1
  Tam<-isolate(Revic(CAUSADEF=Etapa2Data1()[,input$E2CA],
                     RECODCBD=Etapa2Data1()[,input$E2C1],
                     RECODCBD2=Etapa2Data1()[,input$E2C2]))
  return(Tam)
})

output$Etapa2Tabla2 <- renderDataTable({
  if (is.null(input$E2updat1==0)) return(NULL)
  Etapa2Data2()
},options = list(aLengthMenu = c(5, 10, 50), 
                 iDisplayLength = 5))

output$RegRev<-renderPrint({
  if (input$E2updat1==0) return(":-)")
  sum(Etapa2Data2()[,10])})

})
