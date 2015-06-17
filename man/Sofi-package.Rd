\name{Sofi-package}
\alias{Sofi-package}
\alias{Sofi}
\encoding{UTF-8}
\docType{package}
\title{Interfaz (web) interactiva con fines didácticos}
\description{
Este paquete tiene  la finalidad de ayudar a aprender diversos temas académicos de una forma interactiva teniendo ejemplos y la posibilidad de resolver nuevos al mismo tiempo. Apuntes de  clase interactivos.
}
\details{
\tabular{ll}{
Package: \tab Sofi\cr
Type: \tab Package\cr
Version: \tab 0.0.25\cr
Date: \tab 2015-06-16\cr
License: \tab GPL-3\cr
}
Se pretende alcanzar una gran variedad de temas desde probabilidad, estadística, física, etc.
}
\author{
José D. Loera <jose.loera@inegi.org.mx>
}
\references{Conocimiento global}
\keyword{package}
\keyword{Registros}
\keyword{Procesos}
\keyword{Estadisticas}

\examples{
# Esta es una función que lanza diferentes interfaces.

# Ejercicios de distribución
if (interactive()) {
  Estadistica(1)
}

# Para ver Distribuciones de probabilidad
if (interactive()) {
  Estadistica(2)
}

# Menú de Sofi
if (interactive()) {
  Estadistica(3)
}

# Graficas de datos
if (interactive()) {
  Estadistica(4)
}

# Función para evaluar códigos de  defunción.
if (interactive()) {
  Defunciones()
}

# Aquí vemos como usar la función DefCap, ejemplo general.
Capis<-c("P456","C682","M613","F629","X392")
DefCap(Capis)

data(DatPru)
DatCap
InfCap

# Cuando ya tenemos el tamaño de la muestra usamos la función así.
Etapa01(IDm=DatCap[,1], CausaD=DatCap[,2], n=InfCap$n)
}