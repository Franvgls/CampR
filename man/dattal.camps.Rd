% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dattal.camps.r
\name{dattal.camps}
\alias{dattal.camps}
\title{Abundancia estratificada para un rango de talla}
\usage{
dattal.camps(
  gr,
  esp,
  camps,
  dns,
  tmin = 0,
  tmax = 999,
  cor.time = TRUE,
  excl.sect = NA,
  years = TRUE,
  ind = "n",
  ti = TRUE,
  las = 2,
  grid = TRUE,
  es = FALSE,
  bw = TRUE,
  graf = FALSE,
  xpng = 1200,
  ypng = 800,
  ppng = 15
)
}
\arguments{
\item{gr}{Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados}

\item{esp}{Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo}

\item{camps}{campañas (años) a representar en el mapa: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"}

\item{dns}{Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant, Golfo de Cádiz "Arsa" (únicamente para sacar datos al IBTS, no gráficos)gr Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados}

\item{tmin}{Talla mínima}

\item{tmax}{Talla máxima}

\item{cor.time}{Si T corrige las abundancias en función de la duración del lance}

\item{excl.sect}{Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos}

\item{years}{Si T muestra los datos por años, si F por campañas (siguiendo el formato del parámetro camps)}

\item{ind}{Parámetro a representar saca los datos en "p"eso o "n"úmero}

\item{ti}{Si T en el gráfico muestra el nombre de la especie y el rango de tallas comprendido}

\item{las}{Controla el sentido de las etiquetas del gráfico, 2 perpendicular al eje, mejor para etiquetas de años}

\item{grid}{Si plot=T incluye un grid horizontal para comparar mejor los límites de las barras.}

\item{es}{Si T gráfico en castellano, si F gráfico en inglés}

\item{graf}{si F no el gráfico va a pantalla, si nombre fichero va a fichero en el directorio en que está wdf}

\item{xpng}{width archivo png si graf es el nombre del fichero}

\item{ypng}{height archivo png si graf es el nombre del fichero}

\item{ppng}{points png archivo si graf es el nombre del fichero}
}
\value{
Devuelve un vector con nombre con el número estratificado del rango de tallas deseados por campaña/año. Si se solicita plot=TRUE saca un gráfico de barras que muestra la abundancia por año. En peso sólo saca los resultados para una especie.
}
\description{
Extrae los datos de abundancia de una especie o conjunto de especies con un rango de tallas determinado a partir de las distribuciones de talla.También puede mostrar los datos de biomasa a partir de la relación talla-peso
}
\examples{
dattal.camps(2,19,Psh,"Porc",0,15,ind="n",graf="Tararito")
}
\seealso{
{\link{dattal.camp}}
}
