% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtallbarplot.r
\name{dtallbarplot}
\alias{dtallbarplot}
\title{Comparación histograma tallas de una especie entre una campaña y serie histórica}
\usage{
dtallbarplot(gr, esp, camps, dns, camp = NA, excl.sect = NA,
  cor.time = TRUE, ymax = NA, horiz = T, years = TRUE)
}
\arguments{
\item{gr}{Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos 4 equinodermos 5 invertebrados}

\item{esp}{Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo}

\item{camps}{serie histórica de campañas para comparar entre la primera y el total, o entre camps y camp: Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"}

\item{dns}{Elige el origen de las bases de datos: Porcupine "Pnew", Cantábrico "Cant", Golfo de Cádiz "Arsa" (proporciona los datos para Medits pero no saca mapas)}

\item{camp}{campaña para comparar con las serie historica camps, debe ser una de las campañas dentro de la serie. Si NA (por defecto) compara la primera con el conjunto de la serie histórica incluida la primera}

\item{excl.sect}{Sectores a excluir como carácter, se pueden elegir tanto los sectores como estratos}

\item{cor.time}{Si T corrige las abundancias en función de la duración del lance}

\item{ymax}{Valor maximo del eje de la y para ambos gráficos de barras}

\item{horiz}{F por defecto saca el histograma en vertical, se puede poner horizontal haciendolo T}

\item{years}{Si T saca los años como nombre de campaña en los paneles lattice de campañas}
}
\description{
Histograma de la distribución de tallas media estratificada por sexos a partir de ficheros del camp
}
\examples{
dtallbarplot(1,34,Nsh[19:28],"Cnew",excl.sect=c(1),ymax=1.5)
}
\seealso{
Other Distribuciones de tallas: \code{\link{denstall.camp}},
  \code{\link{dtall.lan}}, \code{\link{dtallan.camp}},
  \code{\link{dtallan.peso}}
}
\concept{Distribuciones de tallas}
