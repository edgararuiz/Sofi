
pkg <- c("foreign", "ggplot2", "sampling")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

library(shiny)
library(foreign)
library(ggplot2)
library(sampling)
library(Sofi)

source("helpers.R")
options(shiny.maxRequestSize=1300*1024^2)
options(shiny.deprecation.messages=FALSE)

shinyServer(function(input, output) {
#Etapa 1
##_____________________________________________________________
# Leer Datos
##_____________________________________________________________
####
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
                choices=c("",colnames(datasetInput1())),selected="CAUSADEF")
  })
  
  datasetInput2 <- reactive({
    read.csv(input$file2$datapath, header=input$header, 
             sep=input$sep, quote=input$quote)
  })
  
  output$NomCap <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("Capit","Seleccionar capitulo", 
                choices=c("",colnames(datasetInput2())),selected="Capitulo")
  })
  
  output$NomErr <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("Error","Seleccionar Errores", 
                choices=c("",colnames(datasetInput2())),selected="Errores")
  })
  
  output$NomEsp <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("Esper","Proporci\u00F3n esperada (o antecedentes)", 
                choices=c("",colnames(datasetInput2())),selected="Ps")
  })
  
  output$NomAtn <- renderUI({
    if (is.null(input$file1))
      return(NULL)
    selectInput("TamMu","Tamaño de la muestral", 
                choices=c("",colnames(datasetInput2())),selected="n")
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
    if (is.null(datasetInput2())) return(NULL)
    if(input$En=="ecu"){
      dat<-data.frame(datasetInput51())
      #ns<-input$TamMu
      values$c<-dat[,input$TamMu]}
    else {if(input$En=="man"){
      dat<-data.frame(datasetInput52())
      #ns<-input$TamMu
      values$c<-dat[,input$TamMu]}
          else {dat<-data.frame(datasetInput2())
                #ns<-input$TamMu
                values$c<-dat[,input$TamMu]}}
    N<-datasetInput3()[[2]][,2]
    input$updat2
    #cat("Valor de input$Capit",input$Capit)
    #Capi<-input$Capit
    MuestraGr<-isolate(OptFact(dat[,input$Capit],N,values$c))
    Orig<-data.frame(datasetInput3()[[1]])
    Muestra<-merge(Orig, MuestraGr, by.x="Id",by.y="NT")
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
  sum(datasetInput5()[,input$TamMu])})
output$num51<-renderPrint({
  if (input$updat3==0) return(":-)")
  sum(datasetInput51()[,input$TamMu])})
output$num52<-renderPrint({
  if (input$updat4==0) return(":-)")
  sum(datasetInput52()[,input$TamMu])})

#____________________________________
#_____________________________________

  datasetInput51 <- reactive({
    if(input$updat3==0) {
      dat<-as.data.frame(datasetInput2())
      Npob<-datasetInput3()[[2]][,2]
      Datos<-data.frame(dat,Npob)
      values$a<-Datos
      #for (i in 1:20) {
        #values$a[i,2]<-dat[i,2]
        #values$a[i,3]<-input$bw_P
      #  values$a[i,4]<-optn(values$a[i,5],dat[i,3],dat[i,2])
      #}
      return(Datos)}
      values$a[input$CapEcu,input$Error]<-input$bw_Error
      values$a[input$CapEcu,input$Esper]<-values$a[input$CapEcu,input$Esper]#input$bw_P
      values$a[input$CapEcu,input$TamMu]<-optn(values$a[input$CapEcu,"Npob"],values$a[input$CapEcu,input$Esper],input$bw_Error)
    return(values$a)
  })
  
  datasetInput52 <- reactive({
    if(input$updat4==0) {
      dat<-as.data.frame(datasetInput2())
      Npob<-datasetInput3()[[2]][,2]
      Datos<-data.frame(dat,Npob)
      values$b<-Datos
      return(Datos)}
    values$b[input$CapMod,input$TamMu]<-input$nmanual
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
  
#####
  #_______________________________________________________________
  #Tablas
  #_______________________________________________________________
#####
  output$tabla1 <- renderDataTable({
    if(is.null(input$file1)) return(NULL)
    datasetInput1()
  }, options = list(lengthMenu = c(10, 30, 50), 
                    pageLength = 10))
  
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
  }, options = list(lengthMenu = c(10, 30, 50), 
                    pageLength = 10))

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

####
#_______________________________________________________________
#_______________________________________________________________
#Etapa 2
#_______________________________________________________________
#_______________________________________________________________
####
#Datos
Etapa2Data1 <- reactive({
  read.dbf(input$Etapa2file1$datapath)
})

Etapa2DataTot <- reactive({
  read.csv(input$Etapa2file2$datapath, header=input$header, 
           sep=input$sep, quote=input$quote)
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

output$Etap2Pobla <- renderUI({
  if (is.null(input$Etapa2file2)) return(NULL)
  selectInput("E2Po","Población de la muestra:", 
              choices=c("",colnames(Etapa2DataTot())),selected="Npob")
})

output$Etap2Int3 <- renderUI({
  if (is.null(Etapa2DataInt3())) return(NULL)
  selectInput("E2I3","Variable de color:", 
              choices=c(colnames(Etapa2DataInt3())),selected="Muestra")
})

output$Etap2Int4 <- renderUI({
  if (is.null(Etapa2DataInt4())) return(NULL)
  selectInput("E2I4","Variable de color:", 
              choices=c(colnames(Etapa2DataInt4())),selected="Muestra")
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

Etapa2DataG1 <- reactive({
  if (input$E2updat1==0) return(NULL)
  dat<-cbind(Etapa2Data1(),Etapa2Data2())
  if (!input$E2RevGua){return(dat)}
  Datos<-dat[dat$Rev==1,]
  return(Datos)
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
  E2dat<-as.data.frame(Etapa2Data2(),stringsAsFactors = FALSE)
  #dat<-Etapa4Data2()
  E2dat<-E2dat[as.integer(E2dat[,7])==4,]
  E2dat<-E2dat[E2dat[,1]!=E2dat[,2],]
  E2CapMis<-E2dat[as.integer(E2dat[,4])==as.integer(E2dat[,5]),]
  E2CapDif<-E2dat[as.integer(E2dat[,4])!=as.integer(E2dat[,5]),]
  E2FrecMis<-table(E2CapMis[,1],E2CapMis[,2])
  E2FrecDif<-table(E2CapDif[,1],E2CapDif[,2])
  E2TableMis<-Frecu(E2FrecMis)
  E2TableDif<-Frecu(E2FrecDif)
  list(E2TableMis,E2TableDif)
})

Etapa2DataMis <- reactive({
  Tem<-as.integer(Etapa2Data34()[[1]][,ncol(Etapa2Data34()[[1]])])
  TableD<-subset(Etapa2Data34()[[1]], Tem >= input$E2nMis)
  return(TableD)
})

Etapa2DataDif <- reactive({
  Tem<-as.integer(Etapa2Data34()[[2]][,ncol(Etapa2Data34()[[2]])])
  TableD<-subset(Etapa2Data34()[[2]], Tem >= input$E2nDif)
  return(TableD)
})

Etapa2DataInt3 <- reactive({
  if (input$E2Inter==0) return(NULL)
  Dat<-Etapa2Data2()
  CapAutBien<-cbind(as.integer(Dat[,4]),as.integer(Dat[,8]))
  Datos<-InterVal(CapAutBien,Etapa2DataTot()[,input$E2Po],input$E2ErrorI3)
  return(Datos)
})

Etapa2DataInt4 <- reactive({
  if (input$E2Inter==0) return(NULL)
  Dat<-Etapa2Data2()
  CapAutBien<-cbind(as.integer(Dat[,4]),as.integer(Dat[,9]))
  Datos<-InterVal(CapAutBien,Etapa2DataTot()[,input$E2Po],input$E2ErrorI4)
  return(Datos)
})



#_____________________________________________________________
#Render
#_____________________________________________________________

output$Etapa2Tabla1 <- renderDataTable({
  if (is.null(input$Etapa2file1)) return(NULL)
  Etapa2Data1()
},options = list(lengthMenu = c(5, 10, 50), 
                 pageLength = 5))

output$Etapa2Tabla2 <- renderDataTable({
  if (input$E2updat1==0) return(NULL)
  Etapa2Data2()
},options = list(lengthMenu = c(5, 10, 50), 
                 pageLength = 10))

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

output$Etapa2TablaMis <- renderTable({
  if (is.null(Etapa2Data34())) return(NULL)
  Etapa2DataMis()
})

output$Etapa2TablaDif <- renderTable({
  if (is.null(Etapa2Data34())) return(NULL)
  Etapa2DataDif()
})

output$Etapa2TablaTot <- renderTable({
  if (is.null(input$Etapa2file2)) return(NULL)
  Etapa2DataTot()
})

output$Etapa2Inter3 <- renderTable({
  if (is.null(Etapa2DataInt3())) return(NULL)
  Etapa2DataInt3()
})

output$Etapa2Inter4 <- renderTable({
  if (is.null(Etapa2DataInt4())) return(NULL)
  Etapa2DataInt4()
})

output$E2GI3 <- renderPlot({
  if(is.null(Etapa2DataInt3())) return()
  Int.Conf3<-as.data.frame(Etapa2DataInt3())
  Fcolor<-as.factor(Etapa2DataInt3()[,input$E2I3])
  Int.Conf3<-data.frame(Int.Conf3,Fcolor)
  Graf3D<-ggplot(Int.Conf3, aes(x=factor(Cap), y=P,color=Fcolor))+geom_point()+geom_errorbar(aes(ymin=pinf3, ymax=psup3), width=0.3,size = .8)+theme_bw(base_size=14)
  return(Graf3D)
})

output$E2GI4 <- renderPlot({
  if(is.null(Etapa2DataInt4())) return()
  Int.Conf4<-as.data.frame(Etapa2DataInt4())
  Fcolor<-as.factor(Etapa2DataInt4()[,input$E2I4])
  Int.Conf4<-data.frame(Int.Conf4,Fcolor)
  Graf4D<-ggplot(Int.Conf4, aes(x=factor(Cap), y=P,color=Fcolor))+geom_point()+geom_errorbar(aes(ymin=pinf3, ymax=psup3), width=0.3,size = .8)+theme_bw(base_size=14)
  return(Graf4D)
})

output$E2RegRev<-renderPrint({
  if (input$E2updat1==0) return(":-)")
  sum(as.integer(Etapa2Data2()[,10]))})

####
#_________________________________________________________________
#Guardar Datos
#_________________________________________________________________
####
output$E2DescarRev <- downloadHandler(
  filename = function() {
    paste('Revisión',input$Etapa2file1[1], Sys.Date(), '.zip', sep='_') 
  },
  content = function(file) {
    write.dbf(Etapa2DataG1(), "Rev.dbf")
    zip(zipfile='fbCrawlExport.zip', files="Rev.dbf")
    file.copy("fbCrawlExport.zip", file)
  }
)

output$E2DescarCaso <- downloadHandler(
  filename = function() {
    paste('Casos',input$Etapa2file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa2Data31(), file)
  }
)

output$E2DescarEr3D <- downloadHandler(
  filename = function() {
    paste('Error3D',input$Etapa2file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa2Data32(), file)
  }
)

output$E2DescarEr4D <- downloadHandler(
  filename = function() {
    paste('Error4D',input$Etapa2file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa2Data33(), file)
  }
)

output$E2DescarC4MC <- downloadHandler(
  filename = function() {
    paste('Caso4Mismo',input$Etapa2file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa2DataMis(), file)
  }
)

output$E2DescarC4DF <- downloadHandler(
  filename = function() {
    paste('Caso4Difer',input$Etapa2file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa2DataDif(), file)
  }
)


output$DescarE2Inter3 <- downloadHandler(
  filename = function() {
    paste('InterConf3',input$Etapa2file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa2DataInt3(), file)
  }
)

output$DescarE2Inter4 <- downloadHandler(
  filename = function() {
    paste('InterConf4',input$Etapa2file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa2DataInt4(), file)
  }
)

####
#_______________________________________________________________
#_______________________________________________________________
#Etapa 4 y 5
#_______________________________________________________________
#_______________________________________________________________
####
#Datos

#Datos
Etapa4Data1 <- reactive({
  read.dbf(input$Etapa4file1$datapath)
})

Etapa4DataTot <- reactive({
  read.csv(input$Etapa4file2$datapath, header=input$header, 
           sep=input$sep, quote=input$quote)
})

#_________________________________________________________________
output$Etap4CausaA <- renderUI({
  if (is.null(input$Etapa4file1)) return(NULL)
  selectInput("E4CA","Autom\u00e1tico (generalmente CAUSADEF)", 
              choices=c("",colnames(Etapa4Data1())),selected="CAUSADEF")
})

output$Etap4Causa1 <- renderUI({
  if (is.null(input$Etapa4file1)) return(NULL)
  selectInput("E4C1","Codificador 1 (generalmente RECODCBD)", 
              choices=c("",colnames(Etapa4Data1())),selected="RECODCBD")
})

output$Etap4Causa2 <- renderUI({
  if (is.null(input$Etapa4file1)) return(NULL)
  selectInput("E4C2","Codificador 2 (generalmente RECODCBD2)", 
              choices=c("",colnames(Etapa4Data1())),selected="RECODCBD2")
})

output$Etap4CausaF <- renderUI({
  if (is.null(input$Etapa4file1)) return(NULL)
  selectInput("E4CF","Codificador Final (generalmente COD_SEL)", 
              choices=c("",colnames(Etapa4Data1())),selected="COD_SEL")
})

output$Etap4Pobla <- renderUI({
  if (is.null(input$Etapa4file2)) return(NULL)
  selectInput("E4Po","Población de la muestra:", 
              choices=c("",colnames(Etapa4DataTot())),selected="Npob")
})

output$Etap4Int3 <- renderUI({
  if (is.null(Etapa4DataInt3())) return(NULL)
  selectInput("E4I3","Variable de color:", 
              choices=c(colnames(Etapa4DataInt3())),selected="Muestra")
})

output$Etap4Int4 <- renderUI({
  if (is.null(Etapa4DataInt4())) return(NULL)
  selectInput("E4I4","Variable de color:", 
              choices=c(colnames(Etapa4DataInt4())),selected="Muestra")
})

#__________________________________________________________________
#Reactive
#__________________________________________________________________

Etapa4Data2 <- reactive({
  if (input$E4updat1==0) return(NULL)
  input$E4updat1
  Tam<-isolate(RevicE4(CAUSADEF=Etapa4Data1()[,input$E4CA],
                     RECODCBD=Etapa4Data1()[,input$E4C1],
                     RECODCBD2=Etapa4Data1()[,input$E4C2],
                     COD_SEL=Etapa4Data1()[,input$E4CF]
                     #Ns=Etapa4DataTot()[,input$E4Po]
                     ))
  #Ta<-as.data.frame(Tam)
  return(Tam)
})

Etapa4DataG1 <- reactive({
  if (input$E4updat1==0) return(NULL)
  dat<-cbind(Etapa4Data1(),Etapa4Data2())
  if (!input$E4RevGua){return(dat)}
  Datos<-dat[dat$Rev==1,]
  return(Datos)
})

Etapa4Data31 <- reactive({
  if (is.null(Etapa4Data2())) return(NULL)
  Tabla3<-as.data.frame(matrix(NA,6,4))
  colnames(Tabla3)<-c("Caso","Comparaci\u00f3n","3 d\u00edgitos","4 d\u00edgitos")
  Tabla3[1:5,1]<-1:5
  Tabla3[1,2]<-"Automática = codificador 1 = codificador 2"
  Tabla3[2,2]<-"Automática = codificador 1 != codificador 2"
  Tabla3[3,2]<-"Automática = codificador 2 != codificador 1"
  Tabla3[4,2]<-"Automática != codificador 1 = codificador 2"
  Tabla3[5,2]<-"Automática != codificador 1 != codificador 2"
  Tabla3[6,2]<-"Total"
  dat<-Etapa4Data2()
  for (i in 1:5){
    Tabla3[i,3]<-nrow(dat[dat[,6]==i,])
    Tabla3[i,4]<-nrow(dat[dat[,7]==i,])
  }
  Tabla3[6,3]<-sum(as.integer(Tabla3[1:5,3]))
  Tabla3[6,4]<-sum(as.integer(Tabla3[1:5,4]))
  return(Tabla3)
})

Etapa4Data32 <- reactive({
  if (is.null(Etapa4Data2())) return(NULL)
  dat<-Etapa4Data2()
  #Digit3<-dat[dat[,6]==4,]
  Digit3<-dat[substr(dat[,1],1,3)!=substr(dat[,5],1,3),]
  Table<-table(as.integer(Digit3[,15]),as.integer(Digit3[,17]))
  return(Table)
})

Etapa4Data33 <- reactive({
  if (is.null(Etapa4Data2())) return(NULL)
  dat<-Etapa4Data2()
  #Digit4<-dat[dat[,7]==4,]
  Digit4<-dat[dat[,1]!=dat[,5],]
  Table<-table(as.integer(Digit4[,15]),as.integer(Digit4[,17]))
  return(Table)
})

Etapa4Data34 <- reactive({
  if (is.null(Etapa4Data2())) return(NULL)
  E4dat<-as.data.frame(Etapa4Data2(),stringsAsFactors = FALSE)
  #dat<-Etapa4Data2()
  #E4dat2<-E4dat[as.integer(E4dat[,7])==4,]
  E4dat<-E4dat[E4dat[,1]!=E4dat[,5],]
  E4CapMis<-E4dat[as.integer(E4dat[,15])==as.integer(E4dat[,17]),]
  E4CapDif<-E4dat[as.integer(E4dat[,15])!=as.integer(E4dat[,17]),]
  E4FrecMis<-table(E4CapMis[,1],E4CapMis[,5])
  E4FrecDif<-table(E4CapDif[,1],E4CapDif[,5])
  E4TableMis<-Frecu(E4FrecMis)
  E4TableDif<-Frecu(E4FrecDif)
  list(E4TableMis,E4TableDif)
})

Etapa4DataMis <- reactive({
  Tem<-as.integer(Etapa4Data34()[[1]][,ncol(Etapa4Data34()[[1]])])
  TableM<-subset(Etapa4Data34()[[1]], Tem >= input$E4nMis)
  return(TableM)
})

Etapa4DataDif <- reactive({
  Tem<-as.integer(Etapa4Data34()[[2]][,ncol(Etapa4Data34()[[2]])])
  TableD<-subset(Etapa4Data34()[[2]], Tem >= input$E4nDif)
  return(TableD)
})

Etapa4DataInt3 <- reactive({
  if (input$E4Inter==0) return(NULL)
  Dat<-Etapa4Data2()
  CapAutBien<-cbind(as.integer(Dat[,15]),as.integer(Dat[,8]))
  Datos<-InterVal(CapAutBien,Etapa4DataTot()[,input$E4Po],input$E4ErrorI3)
  return(Datos)
})

Etapa4DataInt4 <- reactive({
  if (input$E4Inter==0) return(NULL)
  Dat<-Etapa4Data2()
  CapAutBien<-cbind(as.integer(Dat[,15]),as.integer(Dat[,9]))
  Datos<-InterVal(CapAutBien,Etapa4DataTot()[,input$E4Po],input$E4ErrorI4)
  return(Datos)
})

#Etapa4DataExp <- reactive({
#  if (input$E4Inter==0) return(NULL)
#  Dat<-PonerFact(Etapa4Data2(),Etapa4DataTot()[,input$E4Po])
#  return(Dat)
#})

Etapa4DataPon3 <- reactive({
  if (input$E4Inter==0) return(NULL)
  Dat<-Etapa4Data2()
  #Dat2<-Dat[substr(Dat[,1],1,3)!=substr(Dat[,5],1,3),]
  #Tabla<-cbind(Dat[,15],Dat[,17],Dat[,27])
  #names(Tabla)<-c("CapAut","CapFin","FactExp")
  #CapAut_Fin<-as.data.frame(Tabla)
  CapAut_Fin<-cbind(Dat[,15],Dat[,17],Dat[,27])
  #CapA<-as.integer(Dat[,15])
  #Tabla<-as.matrix(Etapa4Data32())
  #Datos<-TabPon(CapAut_CapFin,Etapa4DataTot()[,input$E4Po])
  Datos<-TabPon(CapAut_Fin)
  return(CapAut_Fin)
})

#_____________________________________________________________
#Render
#_____________________________________________________________

output$Etapa4Tabla1 <- renderDataTable({
  if (is.null(input$Etapa4file1)) return(NULL)
  Etapa4Data1()
},options = list(lengthMenu = c(5, 10, 50), 
                 pageLength = 5))

output$Etapa4Tabla2 <- renderDataTable({
  if (input$E4updat1==0) return(NULL)
  Etapa4Data2()
},options = list(lengthMenu = c(5, 10, 50), 
                 pageLength = 10))

output$Etapa4Tabla31 <- renderTable({
  if (input$E4updat1==0) return(NULL)
  Etapa4Data31()
})

output$Etapa4Tabla32 <- renderTable({
  if (input$E4updat1==0) return(NULL)
  Etapa4Data32()
})

output$Etapa4Tabla33 <- renderTable({
  if (input$E4updat1==0) return(NULL)
  Etapa4Data33()
})

output$Etapa4TablaMis <- renderTable({
  if (is.null(Etapa4Data34())) return(NULL)
  Etapa4DataMis()
})

output$Etapa4TablaDif <- renderTable({
  if (is.null(Etapa4Data34())) return(NULL)
  Etapa4DataDif()
})

output$Etapa4TablaTot <- renderTable({
  if (is.null(input$Etapa4file2)) return(NULL)
  Etapa4DataTot()
})

output$Etapa4Inter3 <- renderTable({
  if (is.null(Etapa4DataInt3())) return(NULL)
  Etapa4DataInt3()
})

output$Etapa4Inter4 <- renderTable({
  if (is.null(Etapa4DataInt4())) return(NULL)
  Etapa4DataInt4()
})

output$Etapa4TabPon3 <- renderTable({
  if (is.null(Etapa4DataPon3())) return(NULL)
  Etapa4DataPon3()
})



#output$Etapa4Prueba <- renderDataTable({
#  if (input$E4updat1==0) return(NULL)
#  Etapa4DataPon3()
#},options = list(lengthMenu = c(5, 10, 50), 
#                 pageLength = 10))




output$E4GI3 <- renderPlot({
  if(is.null(Etapa4DataInt3())) return()
  Int.Conf3<-as.data.frame(Etapa4DataInt3())
  Fcolor<-as.factor(Etapa4DataInt3()[,input$E4I3])
  Int.Conf3<-data.frame(Int.Conf3,Fcolor)
  Graf3D<-ggplot(Int.Conf3, aes(x=factor(Cap), y=P,color=Fcolor))+geom_point()+geom_errorbar(aes(ymin=pinf3, ymax=psup3), width=0.3,size = .8)+theme_bw(base_size=14)
  return(Graf3D)
})

output$E4GI4 <- renderPlot({
  if(is.null(Etapa4DataInt4())) return()
  Int.Conf4<-as.data.frame(Etapa4DataInt4())
  Fcolor<-as.factor(Etapa4DataInt4()[,input$E4I4])
  Int.Conf4<-data.frame(Int.Conf4,Fcolor)
  Graf4D<-ggplot(Int.Conf4, aes(x=factor(Cap), y=P,color=Fcolor))+geom_point()+geom_errorbar(aes(ymin=pinf3, ymax=psup3), width=0.3,size = .8)+theme_bw(base_size=14)
  return(Graf4D)
})

output$E4RegRev<-renderPrint({
  if (input$E4updat1==0) return(":-)")
  sum(as.integer(Etapa4Data2()[,10]))})

####
#_________________________________________________________________
#Guardar Datos
#_________________________________________________________________
####
output$E4DescarRev <- downloadHandler(
  filename = function() {
    paste('Revisión',input$Etapa4file1[1], Sys.Date(), '.zip', sep='_') 
  },
  content = function(file) {
    write.dbf(Etapa4DataG1(), "Rev.dbf")
    zip(zipfile='fbCrawlExport.zip', files="Rev.dbf")
    file.copy("fbCrawlExport.zip", file)
  }
)

output$E4DescarCaso <- downloadHandler(
  filename = function() {
    paste('Casos',input$Etapa4file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa4Data31(), file)
  }
)

output$E4DescarEr3D <- downloadHandler(
  filename = function() {
    paste('Error3D',input$Etapa4file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa4Data32(), file)
  }
)

output$E4DescarEr4D <- downloadHandler(
  filename = function() {
    paste('Error4D',input$Etapa4file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa4Data33(), file)
  }
)

output$E4DescarC4MC <- downloadHandler(
  filename = function() {
    paste('Caso4Mismo',input$Etapa4file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa4DataMis(), file)
  }
)

output$E4DescarC4DF <- downloadHandler(
  filename = function() {
    paste('Caso4Difer',input$Etapa4file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa4DataDif(), file)
  }
)


output$DescarE4Inter3 <- downloadHandler(
  filename = function() {
    paste('InterConf3',input$Etapa4file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa4DataInt3(), file)
  }
)

output$DescarE4Inter4 <- downloadHandler(
  filename = function() {
    paste('InterConf4',input$Etapa4file1[1], Sys.Date(), '.csv', sep='_') 
  },
  content = function(file) {
    write.csv(Etapa4DataInt4(), file)
  }
)

output$downloadReportE2 <- downloadHandler(
  filename = function() {
    paste('Eta2Rep',input$file[1],Sys.Date(), '.pdf', sep='_')
  },
  content = function(file) {
    rnw <- normalizePath('ReporteE2.Rnw')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    library(knitr)
    out <- knit2pdf(rnw)
    file.rename(out, file)
  }
)


})
