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
    if (is.null(datasetInput6())) return(NULL)
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



#_________________________________________________________________
output$Etap2CausaA <- renderUI({
  if (is.null(input$Etapa2file1)) return(NULL)
  selectInput("E2CA","Autom\u00e1tico (generalmente CAUSADEF)", 
              choices=c("",colnames(Etapa2Data1())),selected="CAUSADEF")
})

output$Etap2Causa1 <- renderUI({
  if (is.null(input$Etapa2file1)) return(NULL)
  selectInput("E2C1","Codificador 1 (generalmente RECODCBD)", 
              choices=c("",colnames(Etapa2Data1())),selected="RECODCBD")
})

output$Etap2Causa2 <- renderUI({
  if (is.null(input$Etapa2file1)) return(NULL)
  selectInput("E2C2","Codificador 2 (generalmente RECODCBD2)", 
              choices=c("",colnames(Etapa2Data1())),selected="RECODCBD2")
})

#__________________________________________________________________
#Reactive
#__________________________________________________________________

Etapa2Data2 <- reactive({
  if (input$E2updat1==0) return(NULL)
  input$E2updat1
  Tam<-isolate(Revic(CAUSADEF=Etapa2Data1()[,input$E2CA],
                     RECODCBD=Etapa2Data1()[,input$E2C1],
                     RECODCBD2=Etapa2Data1()[,input$E2C2]))
  #Ta<-as.data.frame(Tam)
  return(Tam)
})

Etapa2Data31 <- reactive({
  if (is.null(Etapa2Data2())) return(NULL)
  Tabla3<-as.data.frame(matrix(NA,6,4))
  colnames(Tabla3)<-c("Caso","Comparaci\u00f3n","3 d\u00edgitos","4 d\u00edgitos")
  Tabla3[1:5,1]<-1:5
  Tabla3[1,2]<-"Automática = codificador 1 = codificador 2"
  Tabla3[2,2]<-"Automática = codificador 1 != codificador 2"
  Tabla3[3,2]<-"Automática = codificador 2 != codificador 1"
  Tabla3[4,2]<-"Automática != codificador 1 = codificador 2"
  Tabla3[5,2]<-"Automática != codificador 1 != codificador 2"
  Tabla3[6,2]<-"Total"
  dat<-Etapa2Data2()
  for (i in 1:5){
    Tabla3[i,3]<-nrow(dat[dat[,6]==i,])
    Tabla3[i,4]<-nrow(dat[dat[,7]==i,])
  }
  Tabla3[6,3]<-sum(as.integer(Tabla3[1:5,3]))
  Tabla3[6,4]<-sum(as.integer(Tabla3[1:5,4]))
  return(Tabla3)
})

Etapa2Data32 <- reactive({
  if (is.null(Etapa2Data2())) return(NULL)
  dat<-Etapa2Data2()
  Digit3<-dat[dat[,6]==4,]
  Table<-table(as.integer(Digit3[,4]),as.integer(Digit3[,5]))
  return(Table)
})

Etapa2Data33 <- reactive({
  if (is.null(Etapa2Data2())) return(NULL)
  dat<-Etapa2Data2()
  Digit4<-dat[dat[,7]==4,]
  Table<-table(as.integer(Digit4[,4]),as.integer(Digit4[,5]))
  return(Table)
})

Etapa2Data34 <- reactive({
  if (is.null(Etapa2Data2())) return(NULL)
  #dat<-data.frame(cbind(Etapa2Data1(),Etapa2Data2()))
  #da<-cbind(Etapa2Data1(),Etapa2Data2())
  #dat<-as.data.frame(cbind(Etapa2Data1(),Etapa2Data2()),stringsAsFactors = FALSE)
  dat<-as.data.frame(Etapa2Data2(),stringsAsFactors = FALSE)
  dat2<-dat[dat[,7]==4,]
  CapMis<-dat2[dat2[,4]==dat2[,5],]
  CapDif<-dat2[dat2[,4]!=dat2[,5],]
  #dato<-subset(dat,Valor4==4)
  #cat("~~~ Dimension de dat2 ",dim(dat2), " c\u00F3digos de defunci\u00F3n      ~~~ \n")
  #dato$CapAut<-as.integer(dato$CapAut)
  #dato$ManualD<-as.integer(dato$ManualD)
  #as.character
  #datos2<-dato[as.integer(dato[,7])!=as.integer(dato[,8]),]
  #cat("~~~ Dimension de dat3 ",dim(dat3), " c~~~ \n")
  #dat<-Etapa2Data2()
  #dato<-dat[dat[,7]==4,]
  #datos2<-dato[dato[,4]!=dato[,5],]
  #Table<-Frecu(CAUSADEF=datos2[,input$E2CA],
  #             RECODCBD=datos2[,input$E2C1])
  #datos2[,2]<-as.character(datos2[,2])
  #datos2[,4]<-as.character(datos2[,4])
  FrecMis<-table(CapMis[,1],CapMis[,2])
  FrecDif<-table(CapDif[,1],CapDif[,2])
  #cat("~~~ Dimension de FrecDif ",dim(FrecDif), " c~~~ \n")
  #cat("Hola \n")
  TableMis<-Frecu(FrecMis)
  TableDif<-Frecu(FrecDif)
  #cat("Hola23")
  #return(FrecDif)
  list(TableMis,TableDif)
})

Etapa2DataG1 <- reactive({
  if (input$E2updat1==0) return(NULL)
  dat<-cbind(Etapa2Data1(),Etapa2Data2())
  if (!input$RevGua){return(dat)}
  Datos<-dat[dat$Rev==1,]
  return(Datos)
})


#_____________________________________________________________
#Render
#_____________________________________________________________
output$Etapa2Tabla1 <- renderDataTable({
  if (is.null(input$Etapa2file1)) return(NULL)
  Etapa2Data1()
},options = list(aLengthMenu = c(5, 10, 50), 
                 iDisplayLength = 5))

output$Etapa2Tabla2 <- renderDataTable({
  if (is.null(input$E2updat1==0)) return(NULL)
  Etapa2Data2()
},options = list(aLengthMenu = c(5, 10, 50), 
                 iDisplayLength = 5))

output$Etapa2Tabla31 <- renderTable({
  if (input$E2updat1==0) return(NULL)
  Etapa2Data31()
})

output$Etapa2Tabla32 <- renderTable({
  if (input$E2updat1==0) return(NULL)
  Etapa2Data32()
})

output$Etapa2Tabla33 <- renderTable({
  if (input$E2updat1==0) return(NULL)
  Etapa2Data33()
})

output$Etapa2Tabla34 <- renderTable({
  if (is.null(input$E2updat1==0)) return(NULL)
  Etapa2Data34()[[1]]
})

output$RegRev<-renderPrint({
  if (input$E2updat1==0) return(":-)")
  sum(as.integer(Etapa2Data2()[,10]))})



#_________________________________________________________________
#Guardar Datos
#_________________________________________________________________

output$DescarRev <- downloadHandler(
  filename = function() {
    paste('Revisión',input$Etapa2file1[1], Sys.Date(), '.zip', sep='_') 
  },
  content = function(file) {
    write.dbf(Etapa2DataG1(), "Rev.dbf")
    zip(zipfile='fbCrawlExport.zip', files="Rev.dbf")
    file.copy("fbCrawlExport.zip", file)
  }
)

})
