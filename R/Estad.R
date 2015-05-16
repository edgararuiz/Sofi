Estad <- function(Nivel){
  if (Nivel==1) {shinyAppDir(system.file("Estadist/Distrib", package="Sofi"))}
  else if (Nivel==2) {shinyAppDir(system.file("Estadist/ggMarginal", package="Sofi"))}
}
