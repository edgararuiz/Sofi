renderText({
  if (is.null(Valor()) | is.null(Valor_Cues)) return(NULL)
  else{
    #text1 = "¿Crees tener la respuesta correcta? Enviarla "
    #as.numeric(Valor_Final())
 
    #errr<-abs(Valor()-input$Res_Cuest)
    if (length(errr)==0) {return(NULL)}
    else if (errr<0.005) {text1 = "Felicidades tu respuesta es correcta"}
    else text1 = paste("Incorrecto, el valor correcto es: = ",signif(Valor_Cues,3))
    return(text1)
  }
  })