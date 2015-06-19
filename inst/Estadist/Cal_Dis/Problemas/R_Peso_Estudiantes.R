renderUI({
  if (!input$Ayuda_visible) return()
  
  if(Bus_Area %in% c("lower","upper")) {
    withMathJax(
      helpText('Es simple, lo primero es solo sustituir los valores de la siguiente forma: 
               $$Z=\\frac { X-\\mu  }{ \\sigma  }=\\frac { ',Cues_Val,'-\\',Peso,'  }{ \\',DS_Peso,' }=
               ',signif((Cues_Val-Peso)/DS_Peso,4),' $$')
      )
  }
  else {
    withMathJax(
      helpText('Es simple, lo primero es solo sustituir los valores de la siguiente forma \\(\\ { Z }_{ 1 }\\): 
               $${ Z }_{ 1 }=\\frac { X-\\mu  }{ \\sigma  }=\\frac { ',a_val,'-\\',Peso,'  }{ \\',DS_Peso,' }=
               ',signif((a_val-Peso)/DS_Peso,4),' $$ Y para \\(\\ { Z }_{ 2 }\\): $${ Z }_{ 2 }=\\frac { X-\\mu  }{ \\sigma  }=\\frac { ',b_val,'-\\',Peso,'  }{ \\',DS_Peso,' }=
               ',signif((b_val-Peso)/DS_Peso,4),' $$')
      )
  }
  
  
  
})



#a_val<<-min(Cues_Val)
#b_val<<-max(Cues_Val)