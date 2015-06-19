\name{Estadistica}
\alias{Estadistica}
\encoding{UTF-8}
\title{
Para ejecutar diferentes niveles de estudio en estadística.
}
\description{
Es la función principal para ejecutar los diversos niveles de estadística, por lo pronto solo cuatro de prueba.
}
\usage{
Estadistica(Nivel)
}
\arguments{
  \item{Nivel}{
  Para los parámetros
  \tabular{ll}{
1 = \tab Ejercicios de distribución\cr
2 = \tab Distribuciones de probabilidad\cr
3 = \tab Menú de Sofi\cr
4 = \tab Gráficas de datos\cr
}
}
}
\details{
Para más detalles ver los ejemplos que se describirán abajo o visitar \href{http://www.inegi.info/sofi}{Sofi}
}
\value{
No se tienen valores de salida
}
\references{
 
  Fuentes de donde se tomo algo de código:
  \tabular{ll}{
+  \tab Del repositorio \href{https://github.com/ShinyEd/ShinyEd}{ShinyEd} de la autora 
\href{https://github.com/mine-cetinkaya-rundel}{Mine Cetinkaya-Rundel}\cr
+  \tab Del repositorio \href{https://github.com/ua-snap/shiny-apps}{shiny-apps} de el autor
\href{https://github.com/leonawicz}{Matthew Leonawicz}\cr
+  \tab Del repositorio \href{https://github.com/daattali/ggExtra/tree/master/inst/examples/ggMarginal}{ggMarginal} de el autor
\href{https://github.com/daattali}{Dean Attali}\cr
}
}
\author{
José D. Loera
}

\examples{
## Esta es una función que lanza diferentes interfaces.

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
}
\keyword{ Graficas }
\keyword{ Estadística }
