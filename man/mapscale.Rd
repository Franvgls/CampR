% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapscale.R
\name{mapscale}
\alias{mapscale}
\title{Auxiliar para incluir escala en un mapa, tomada de paquete maps pero con opción para elejir español (es=T) o inglés (es=F)}
\usage{
mapscale(x, y, relwidth = 0.15, metric = TRUE, ratio = TRUE, es = F, ...)
}
\arguments{
\item{x}{Define coordinada horizontal de inserción de la escala}

\item{y}{Define coordinada horizontal de inserción de la escala}

\item{relwidth}{proporción horizontal de visualización que se utilizará para la escala. El valor predeterminado es 0,15 (15\%).}

\item{metric}{Si T la unidad será en km, si falso en millas}

\item{es}{Si es TRUE texto de la escala en español, si FALSE (por defecto) en inglés}

\item{...}{otros parámetros graficos}
}
\value{
Incluye la escala en el mapa
}
\description{
Función auxiliar para los mapas
}
\examples{
MapArsa(escala=TRUE,cex.scala=.9,es=TRUE)
MapNort(escala=T,cex.scala=.6,es=T)
}
\references{
maps::map.scale function pone una escala en un mapa, en esta función se ha añadido la opción de idioma \code{\link[maps]{map.scale}}#' @seealso {\link{MapNort}}, {\link{MapArsa}}
}
\seealso{
Other mapas base: 
\code{\link{MapArsa}()},
\code{\link{MapCant}()},
\code{\link{MapIberia}()},
\code{\link{MapMedit}()},
\code{\link{MapNort}()},
\code{\link{MapPorc}()},
\code{\link{mapporco}()}
}
\concept{mapas base}
