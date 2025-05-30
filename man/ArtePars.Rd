% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ArtePars.R
\name{ArtePars}
\alias{ArtePars}
\title{Gráficos de parámetros del arte con la profundidad}
\usage{
ArtePars(
  camp,
  dns = "Cant",
  incl2 = TRUE,
  es = T,
  bw = TRUE,
  ti = TRUE,
  sub = NULL,
  out.dat = FALSE,
  ceros = TRUE,
  cex.leg = 1.1,
  years = TRUE,
  profrange = NA,
  proflab = F,
  Nlans = TRUE,
  lan.cex = 0.8,
  graf = FALSE,
  xpng = 1200,
  ypng = 800,
  ppng = 15
)
}
\arguments{
\item{camp}{Campaña a representar en el mapa de un año concreto (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"}

\item{dns}{Elige el origen de las bases de datos: Porcupine "Porc" o  "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)}

\item{incl2}{Si F no tiene en cuenta los lances especiales, si T si los tiene en cuenta, pero da problemas por que no puede calcular las abundancias estratificadas}

\item{es}{Si T rotulos gráfico en español, si F en inglés}

\item{bw}{Gráfico en blanco en negro si T o en color si F}

\item{ti}{Añade el nombre de la especie en latín sin T, si F no añade titulo}

\item{sub}{Añade un subtítulo debajo del gráfico, sin texto por defecto.}

\item{out.dat}{Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto}

\item{ceros}{por defecto incluye los valores de 0 de los parámetros}

\item{cex.leg}{Varía el tamaño de letra de los ejes y del número de la leyenda}

\item{years}{Si T saca los años como nombre de campaña en los paneles lattice de campañas}

\item{profrange}{Si c(profmin,profmax) filtra por ese rango de profundidad por defecto NA no filtra profundidades}

\item{lan.cex}{tamaño de las etiquetas del numero de lances por campaña}

\item{graf}{si F no el gráfico va a pantalla, si nombre fichero va a fichero en el directorio en que está wdf}

\item{xpng}{width archivo png si graf es el nombre del fichero}

\item{ypng}{height archivo png si graf es el nombre del fichero}

\item{ppng}{points png archivo si graf es el nombre del fichero}

\item{plot}{Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)}

\item{escmult}{Varía la relación de tamaño de los puntos con la leyenda y el máximo en los datos}
}
\value{
Si out.dat=TRUE devuelve un data.frame con columnas: lan,lat,long,prof,peso.gr,numero (de individuos entre tmin y tmax),camp, si out.dat=F saca el gráfico en pantalla o como objeto para combinar con otros gráficos con print.trellis
}
\description{
Crea gráficos del comportamiento del arte en los lances de una campaña con la profundidad
}
\examples{
ArtePars("N23","Cant",Nlans = F,ti=T)
ArtePars("N23","Cant",Nlans = T,ti=T,lan.cex=2)
}
\seealso{
Other gear: 
\code{\link{ArteParCompC}()},
\code{\link{ArteParCompD}()},
\code{\link{ArteParCompV}()}
}
\concept{gear}
