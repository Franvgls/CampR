\name{Nort.map}
\alias{Nort.map}
\docType{data}
\title{
Estructura del mapa de la demarcacion Noratlantica 
}
\description{Contiene los estratos en los que se divide la campaña Demersales plataforma Norte
}
\format{ lista con cuatro objetos:
 \itemize{
  \item x: longitud en grados decimales con los puntos que definen cada uno de los estratos, debe contener las lineas que definen cada uno de los estratos definidos en names
  \item { y: latitud en grados decimales igual que x
  \item {range: puntos que definen extremos MinX,MaxX,MinY,MaxY
  \item {names: Nombres de los estratos definidos en X e Y
  \item {attr: clase map}
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
str(Nort.map)
plot(Nort.map,type="l") 
}
\keyword{datasets}
