\name{Porc.map}
\alias{Porc.map}
\docType{data}
\title{
Estructura del mapa del area de estudio de Porcupine 
}
\description{Contiene los estratos en los que se divide la campaña de arrastre Porcupine
}
\format{ lista con cuatro objetos:
 \itemize{
  \item {x: longitud en grados decimales con los puntos que definen cada uno de los estratos, debe contener las lineas que definen cada uno de los estratos definidos en names
  \item { y: latitud en grados decimales igual que x
  \item {range: puntos que definen extremos MinX,MaxX,MinY,MaxY
  \item {names: Nombres de los estratos definidos en X e Y
  \item {attr: clase map}
}
}
}
}
}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
str(Porc.map)
plot(Porc.map,type="l") 
}
\keyword{datasets}
