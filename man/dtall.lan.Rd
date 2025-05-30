% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtall.lan.r
\name{dtall.lan}
\alias{dtall.lan}
\title{Histograma de distribución de tallas de un lance concreto o la distribución media en varios lances}
\usage{
dtall.lan(
  gr,
  esp,
  camp,
  dns = "Cant",
  lances = NA,
  cor.time = TRUE,
  depth_range = c(NA),
  ti = FALSE,
  legend = TRUE,
  cexleg = 1,
  bw = TRUE,
  es = TRUE,
  sex = TRUE,
  muestr = TRUE,
  MeanLan = TRUE,
  plot = TRUE,
  idi = "l",
  ymax = NA,
  out.dat = FALSE
)
}
\arguments{
\item{gr}{Grupo de la especie: 1 peces, 2 crustáceos 3 moluscos (4 equinodermos y 5 invertebrados normalmente no medidos)}

\item{esp}{Código de la especie numérico o carácter con tres espacios. 999 para todas las especies del grupo}

\item{camp}{Campaña con el lance a representar en el histograma (XX): Demersales "NXX", Porcupine "PXX", Arsa primavera "1XX" y Arsa otoño "2XX"}

\item{dns}{Elige el origen de las bases de datos: Porcupine "Porc" o "Pnew", Cantábrico "Cant", Golfo de Cadiz "Arsa" (proporciona los datos para Medits pero no saca mapas)}

\item{lances}{Lance o lances de los que se quiere sacar la distribución de tallas. Si NA coge todos los lances de la campaña.}

\item{cor.time}{T por defecto, si T corrige los lances con tiempo distinto de 30'}

\item{depth_range}{NA por defecto, si no poner dos valores, min y max para establecer los límites batimétricos de los lances. Como subtítulo a pie de gráfica saca el rango de profundidades buscado.}

\item{ti}{Si T añade título al gráfico, el nombre de la especie en latín.}

\item{legend}{Si T (por defecto) añade la leyenda, si no se saca por sexos no añade información}

\item{cexleg}{Varía el tamaño de letra de los ejes y del número de la leyenda}

\item{bw}{Gráfico en blanco en negro si T o en color si F.}

\item{es}{Si T gráfico en castellano, si F gráfico en inglés.}

\item{sex}{por defecto (T) da valores por sexos si los hay, si solo hay indeterminados funciona igual.}

\item{muestr}{Por defecto (T) no pondera los datos por el peso total en la captura del lance, si T coge los medidos realmente}

\item{MeanLan}{por defecto (T) saca la media por lance, dividiendo por el número de lances utilizados}

\item{plot}{Saca el gráfico (T) o lo guarda como objeto para componer con otros gráficos (F)}

\item{idi}{Nombre científico de la especie ("l") o nombre común ("e").}

\item{ymax}{permite establecer el valor máximo de las ordenadas en el gráfico.Por defecto (NA) se ajusta al valor máximo del número de individuos.}

\item{out.dat}{Si T el resultado final de la función es la figura en pantalla, pero los datos en objeto}
}
\value{
Saca el gráfico en pantalla, para sacar datos utilizar {\link{dattal.camp}}
}
\description{
Dibuja el histograma de la distribución de tallas de la especie por sexos (si existen). Si se selecciona más de un lance es la distribución de tallas media en los lances seleccionados, si sólo hay un lance es la total del lance.
}
\examples{
dtall.lan(1,36,"P08","Porc",lances=c(10:15,17),ti=TRUE)
}
\seealso{
Other Distribuciones de tallas: 
\code{\link{denstall.camp}()},
\code{\link{dtall.campa}()},
\code{\link{dtall.camp}()},
\code{\link{dtallan.camp}()},
\code{\link{dtallan.peso}()},
\code{\link{dtallbarplot}()}
}
\concept{Distribuciones de tallas}
